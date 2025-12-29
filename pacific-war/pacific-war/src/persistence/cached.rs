use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;
use async_trait::async_trait;
use crate::core::Battle;
use super::{BattleRepository, PersistenceError, Result};

/// Write-through cache wrapper for any BattleRepository implementation
///
/// This wrapper provides:
/// - Lazy loading: battles are loaded from the repository on first access
/// - Write-through caching: all updates are immediately persisted to the underlying repository
/// - Fast reads: cached battles are served from memory without database access
pub struct CachedRepository<R: BattleRepository> {
    cache: Arc<RwLock<HashMap<Uuid, Battle>>>,
    repository: R,
}

impl<R: BattleRepository> CachedRepository<R> {
    /// Create a new cached repository wrapping the given repository
    pub fn new(repository: R) -> Self {
        Self {
            cache: Arc::new(RwLock::new(HashMap::new())),
            repository,
        }
    }

    /// Get from cache only (for read-heavy operations)
    ///
    /// Returns None if the battle is not in cache.
    /// Use `get()` to load from repository on cache miss.
    pub async fn get_from_cache(&self, id: &Uuid) -> Option<Battle> {
        let cache = self.cache.read().await;
        cache.get(id).cloned()
    }

    /// Get the underlying cache for direct access (used in tests)
    #[cfg(test)]
    pub async fn cache_size(&self) -> usize {
        let cache = self.cache.read().await;
        cache.len()
    }
}

#[async_trait]
impl<R: BattleRepository> BattleRepository for CachedRepository<R> {
    async fn create(&self, battle: &Battle) -> Result<()> {
        // Write-through: persist first to ensure durability
        self.repository.create(battle).await?;

        // Then update cache
        let mut cache = self.cache.write().await;
        cache.insert(battle.id, battle.clone());

        Ok(())
    }

    async fn get(&self, id: Uuid) -> Result<Battle> {
        // Check cache first
        {
            let cache = self.cache.read().await;
            if let Some(battle) = cache.get(&id) {
                return Ok(battle.clone());
            }
        }

        // Cache miss: load from repository
        let battle = self.repository.get(id).await?;

        // Update cache
        let mut cache = self.cache.write().await;
        cache.insert(id, battle.clone());

        Ok(battle)
    }

    async fn update(&self, battle: &Battle) -> Result<()> {
        // Write-through: persist first to ensure durability
        self.repository.update(battle).await?;

        // Update cache with incremented version
        // Note: The database increments version, so we do the same in cache
        let mut cache = self.cache.write().await;
        let mut updated_battle = battle.clone();
        updated_battle.version += 1;
        cache.insert(battle.id, updated_battle);

        Ok(())
    }

    async fn delete(&self, id: Uuid) -> Result<()> {
        // Write-through: persist first
        self.repository.delete(id).await?;

        // Remove from cache
        let mut cache = self.cache.write().await;
        cache.remove(&id);

        Ok(())
    }

    async fn health_check(&self) -> Result<()> {
        self.repository.health_check().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::persistence::InMemoryRepository;
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
    async fn test_create_caches_battle() {
        let repo = InMemoryRepository::new();
        let cached = CachedRepository::new(repo);

        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        cached.create(&battle).await.unwrap();

        // Battle should be in cache
        assert_eq!(cached.cache_size().await, 1);
        let cached_battle = cached.get_from_cache(&battle_id).await.unwrap();
        assert_eq!(cached_battle.id, battle_id);
    }

    #[tokio::test]
    async fn test_get_loads_into_cache() {
        let repo = InMemoryRepository::new();
        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        // Create directly in repository
        repo.create(&battle).await.unwrap();

        let cached = CachedRepository::new(repo);

        // Cache should be empty initially
        assert_eq!(cached.cache_size().await, 0);

        // Get should load into cache
        let retrieved = cached.get(battle_id).await.unwrap();
        assert_eq!(retrieved.id, battle_id);

        // Now cache should have it
        assert_eq!(cached.cache_size().await, 1);
    }

    #[tokio::test]
    async fn test_update_updates_cache() {
        let repo = InMemoryRepository::new();
        let cached = CachedRepository::new(repo);

        let battle_id = Uuid::new_v4();
        let mut battle = test_battle(battle_id);

        cached.create(&battle).await.unwrap();
        assert_eq!(battle.version, 0);

        // Update battle
        battle.next();
        cached.update(&battle).await.unwrap();

        // Cache should have updated version
        let cached_battle = cached.get_from_cache(&battle_id).await.unwrap();
        assert_eq!(cached_battle.version, 1);
    }

    #[tokio::test]
    async fn test_delete_removes_from_cache() {
        let repo = InMemoryRepository::new();
        let cached = CachedRepository::new(repo);

        let battle_id = Uuid::new_v4();
        let battle = test_battle(battle_id);

        cached.create(&battle).await.unwrap();
        assert_eq!(cached.cache_size().await, 1);

        cached.delete(battle_id).await.unwrap();

        // Cache should be empty
        assert_eq!(cached.cache_size().await, 0);
        assert!(cached.get_from_cache(&battle_id).await.is_none());
    }

    #[tokio::test]
    async fn test_get_from_cache_returns_none_on_miss() {
        let repo = InMemoryRepository::new();
        let cached = CachedRepository::new(repo);

        let battle_id = Uuid::new_v4();
        assert!(cached.get_from_cache(&battle_id).await.is_none());
    }
}
