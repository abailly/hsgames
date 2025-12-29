use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;
use async_trait::async_trait;
use crate::core::Battle;
use super::{BattleRepository, PersistenceError, Result};

/// In-memory implementation of BattleRepository for testing
///
/// This implementation stores battles in a HashMap with no actual persistence.
/// It's useful for unit tests and integration tests that don't require a real database.
pub struct InMemoryRepository {
    battles: Arc<RwLock<HashMap<Uuid, Battle>>>,
}

impl InMemoryRepository {
    /// Create a new empty in-memory repository
    pub fn new() -> Self {
        Self {
            battles: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Get the number of battles in the repository (for testing)
    #[cfg(test)]
    pub async fn len(&self) -> usize {
        let battles = self.battles.read().await;
        battles.len()
    }

    /// Check if the repository is empty (for testing)
    #[cfg(test)]
    pub async fn is_empty(&self) -> bool {
        let battles = self.battles.read().await;
        battles.is_empty()
    }
}

impl Default for InMemoryRepository {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl BattleRepository for InMemoryRepository {
    async fn create(&self, battle: &Battle) -> Result<()> {
        let mut battles = self.battles.write().await;

        // Check if battle already exists
        if battles.contains_key(&battle.id) {
            return Err(PersistenceError::DatabaseError(
                format!("Battle with id {} already exists", battle.id)
            ));
        }

        battles.insert(battle.id, battle.clone());
        Ok(())
    }

    async fn get(&self, id: Uuid) -> Result<Battle> {
        let battles = self.battles.read().await;

        battles.get(&id)
            .cloned()
            .ok_or(PersistenceError::NotFound(id))
    }

    async fn update(&self, battle: &Battle) -> Result<()> {
        let mut battles = self.battles.write().await;

        // Get existing battle
        let existing = battles.get(&battle.id)
            .ok_or(PersistenceError::NotFound(battle.id))?;

        // Optimistic locking check
        if existing.version != battle.version {
            return Err(PersistenceError::ConcurrentModification(battle.id));
        }

        // Update with incremented version
        let mut updated = battle.clone();
        updated.version += 1;
        battles.insert(battle.id, updated);

        Ok(())
    }

    async fn delete(&self, id: Uuid) -> Result<()> {
        let mut battles = self.battles.write().await;

        battles.remove(&id)
            .map(|_| ())
            .ok_or(PersistenceError::NotFound(id))
    }

    async fn health_check(&self) -> Result<()> {
        // In-memory repository is always healthy
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::NewBattle;
    use chrono::NaiveDate;
    use crate::util::NaiveDateForm;

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

    #[tokio::test]
    async fn test_create_and_get() {
        let repo = InMemoryRepository::new();
        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        repo.create(&battle).await.unwrap();

        let retrieved = repo.get(battle_id).await.unwrap();
        assert_eq!(retrieved.id, battle_id);
        assert_eq!(retrieved.version, 0);
    }

    #[tokio::test]
    async fn test_create_duplicate_fails() {
        let repo = InMemoryRepository::new();
        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        repo.create(&battle).await.unwrap();

        // Try to create again
        let result = repo.create(&battle).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_get_not_found() {
        let repo = InMemoryRepository::new();
        let battle_id = Uuid::new_v4();

        let result = repo.get(battle_id).await;
        assert!(matches!(result, Err(PersistenceError::NotFound(_))));
    }

    #[tokio::test]
    async fn test_update() {
        let repo = InMemoryRepository::new();
        let battle_id = Uuid::new_v4();
        let mut battle = test_battle(battle_id);

        repo.create(&battle).await.unwrap();

        // Update battle
        battle.next();
        repo.update(&battle).await.unwrap();

        let updated = repo.get(battle_id).await.unwrap();
        assert_eq!(updated.version, 1);
    }

    #[tokio::test]
    async fn test_update_not_found() {
        let repo = InMemoryRepository::new();
        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        let result = repo.update(&battle).await;
        assert!(matches!(result, Err(PersistenceError::NotFound(_))));
    }

    #[tokio::test]
    async fn test_concurrent_modification() {
        let repo = InMemoryRepository::new();
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

    #[tokio::test]
    async fn test_delete() {
        let repo = InMemoryRepository::new();
        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        repo.create(&battle).await.unwrap();
        assert_eq!(repo.len().await, 1);

        repo.delete(battle_id).await.unwrap();
        assert!(repo.is_empty().await);
    }

    #[tokio::test]
    async fn test_delete_not_found() {
        let repo = InMemoryRepository::new();
        let battle_id = Uuid::new_v4();

        let result = repo.delete(battle_id).await;
        assert!(matches!(result, Err(PersistenceError::NotFound(_))));
    }

    #[tokio::test]
    async fn test_health_check() {
        let repo = InMemoryRepository::new();
        assert!(repo.health_check().await.is_ok());
    }
}
