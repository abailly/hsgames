use sqlx::{PgPool, postgres::PgPoolOptions};
use serde_json;
use async_trait::async_trait;
use uuid::Uuid;
use crate::core::Battle;
use super::{BattleRepository, PersistenceError, Result};

pub struct PostgresRepository {
    pool: PgPool,
}

impl PostgresRepository {
    /// Create a new PostgreSQL repository with the given database URL
    pub async fn new(database_url: &str) -> Result<Self> {
        let pool = PgPoolOptions::new()
            .max_connections(10)
            .min_connections(2)
            .connect(database_url)
            .await
            .map_err(|e| PersistenceError::DatabaseError(format!("Failed to connect to database: {}", e)))?;

        Ok(Self { pool })
    }

    /// Run database migrations
    pub async fn run_migrations(&self) -> Result<()> {
        sqlx::migrate!("./migrations")
            .run(&self.pool)
            .await
            .map_err(|e| PersistenceError::DatabaseError(format!("Failed to run migrations: {}", e)))?;

        Ok(())
    }
}

#[async_trait]
impl BattleRepository for PostgresRepository {
    async fn create(&self, battle: &Battle) -> Result<()> {
        let json = serde_json::to_value(battle)?;

        sqlx::query(
            "INSERT INTO battles (id, battle_state, version)
             VALUES ($1, $2, 0)"
        )
        .bind(battle.id)
        .bind(json)
        .execute(&self.pool)
        .await
        .map_err(|e| PersistenceError::DatabaseError(format!("Failed to create battle: {}", e)))?;

        Ok(())
    }

    async fn get(&self, id: Uuid) -> Result<Battle> {
        let row: (serde_json::Value, i32) = sqlx::query_as(
            "SELECT battle_state, version FROM battles WHERE id = $1"
        )
        .bind(id)
        .fetch_one(&self.pool)
        .await
        .map_err(|e: sqlx::Error| -> PersistenceError {
            match e {
                sqlx::Error::RowNotFound => PersistenceError::NotFound(id),
                _ => PersistenceError::DatabaseError(format!("Failed to get battle: {}", e)),
            }
        })?;

        let mut battle: Battle = serde_json::from_value(row.0)
            .map_err(|e| PersistenceError::SerializationError(format!("Failed to deserialize battle: {}", e)))?;

        // Use the version column as the source of truth
        battle.version = row.1;

        Ok(battle)
    }

    async fn update(&self, battle: &Battle) -> Result<()> {
        // Increment version in the battle struct for consistency
        let mut updated_battle = battle.clone();
        updated_battle.version += 1;
        let json = serde_json::to_value(&updated_battle)?;

        // Optimistic locking: only update if version matches
        let result = sqlx::query(
            "UPDATE battles
             SET battle_state = $1, version = version + 1
             WHERE id = $2 AND version = $3"
        )
        .bind(json)
        .bind(battle.id)
        .bind(battle.version)
        .execute(&self.pool)
        .await
        .map_err(|e| PersistenceError::DatabaseError(format!("Failed to update battle: {}", e)))?;

        if result.rows_affected() == 0 {
            // Check if battle exists
            let exists: Option<(Uuid,)> = sqlx::query_as("SELECT id FROM battles WHERE id = $1")
                .bind(battle.id)
                .fetch_optional(&self.pool)
                .await
                .map_err(|e: sqlx::Error| -> PersistenceError {
                    PersistenceError::DatabaseError(format!("Failed to check battle existence: {}", e))
                })?;

            if exists.is_some() {
                return Err(PersistenceError::ConcurrentModification(battle.id));
            } else {
                return Err(PersistenceError::NotFound(battle.id));
            }
        }

        Ok(())
    }

    async fn delete(&self, id: Uuid) -> Result<()> {
        let result = sqlx::query("DELETE FROM battles WHERE id = $1")
            .bind(id)
            .execute(&self.pool)
            .await
            .map_err(|e| PersistenceError::DatabaseError(format!("Failed to delete battle: {}", e)))?;

        if result.rows_affected() == 0 {
            return Err(PersistenceError::NotFound(id));
        }

        Ok(())
    }

    async fn health_check(&self) -> Result<()> {
        sqlx::query("SELECT 1")
            .execute(&self.pool)
            .await
            .map_err(|e| PersistenceError::DatabaseError(format!("Health check failed: {}", e)))?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::NewBattle;
    use chrono::NaiveDate;
    use crate::util::NaiveDateForm;

    // Test helper to create a test battle
    fn test_battle(id: Uuid) -> Battle {
        let new_battle = NewBattle {
            battle_name: "Test Battle".to_string(),
            start_date: NaiveDateForm {
                date: NaiveDate::from_ymd_opt(1942, 5, 1).unwrap(),
            },
            duration: 21,
            operation_player: crate::core::Side::Japan,
            intelligence_condition: crate::core::Intelligence::Intercept,
        };
        Battle::new(id, &new_battle)
    }

    // Note: These tests require a PostgreSQL database running
    // Set DATABASE_URL environment variable to run them (e.g., DATABASE_URL=postgres://localhost/pacific_war_test cargo test)
    #[tokio::test]
    async fn test_create_and_retrieve_battle() {
        let database_url = match std::env::var("DATABASE_URL") {
            Ok(url) => url,
            Err(_) => {
                eprintln!("Skipping test_create_and_retrieve_battle: DATABASE_URL not set");
                return;
            }
        };

        let repo = PostgresRepository::new(&database_url).await.unwrap();
        repo.run_migrations().await.unwrap();

        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        repo.create(&battle).await.unwrap();
        let retrieved = repo.get(battle_id).await.unwrap();

        assert_eq!(battle.id, retrieved.id);
        assert_eq!(battle.version, retrieved.version);
    }

    #[tokio::test]
    async fn test_update_increments_version() {
        let database_url = match std::env::var("DATABASE_URL") {
            Ok(url) => url,
            Err(_) => {
                eprintln!("Skipping test_update_increments_version: DATABASE_URL not set");
                return;
            }
        };

        let repo = PostgresRepository::new(&database_url).await.unwrap();
        repo.run_migrations().await.unwrap();

        let battle_id = Uuid::new_v4();
        let mut battle = test_battle(battle_id);

        repo.create(&battle).await.unwrap();
        assert_eq!(battle.version, 0);

        battle.next();
        repo.update(&battle).await.unwrap();

        let updated = repo.get(battle_id).await.unwrap();
        assert_eq!(updated.version, 1);
    }

    #[tokio::test]
    async fn test_concurrent_modification_detection() {
        let database_url = match std::env::var("DATABASE_URL") {
            Ok(url) => url,
            Err(_) => {
                eprintln!("Skipping test_concurrent_modification_detection: DATABASE_URL not set");
                return;
            }
        };

        let repo = PostgresRepository::new(&database_url).await.unwrap();
        repo.run_migrations().await.unwrap();

        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        repo.create(&battle).await.unwrap();

        // Simulate concurrent update
        let mut battle1 = repo.get(battle_id).await.unwrap();
        let mut battle2 = repo.get(battle_id).await.unwrap();

        battle1.next();
        repo.update(&battle1).await.unwrap();

        battle2.next();
        let result = repo.update(&battle2).await;

        assert!(matches!(result, Err(PersistenceError::ConcurrentModification(_))));
    }
}
