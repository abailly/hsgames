use async_trait::async_trait;
use uuid::Uuid;
use crate::core::Battle;

#[derive(Debug, Clone)]
pub enum PersistenceError {
    NotFound(Uuid),
    SerializationError(String),
    DatabaseError(String),
    ConcurrentModification(Uuid),
}

impl std::fmt::Display for PersistenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::NotFound(id) => write!(f, "Battle not found: {}", id),
            Self::SerializationError(msg) => write!(f, "Serialization error: {}", msg),
            Self::DatabaseError(msg) => write!(f, "Database error: {}", msg),
            Self::ConcurrentModification(id) => write!(f, "Concurrent modification detected for battle: {}", id),
        }
    }
}

impl std::error::Error for PersistenceError {}

// Convert from serde_json errors
impl From<serde_json::Error> for PersistenceError {
    fn from(err: serde_json::Error) -> Self {
        PersistenceError::SerializationError(err.to_string())
    }
}

// Convert from sqlx errors
impl From<sqlx::Error> for PersistenceError {
    fn from(err: sqlx::Error) -> Self {
        match err {
            sqlx::Error::RowNotFound => {
                // UUID will be added by caller context
                PersistenceError::DatabaseError("Row not found".to_string())
            }
            _ => PersistenceError::DatabaseError(err.to_string())
        }
    }
}

// Convert to Rocket Status for HTTP responses
impl From<PersistenceError> for rocket::http::Status {
    fn from(err: PersistenceError) -> Self {
        match err {
            PersistenceError::NotFound(_) => rocket::http::Status::NotFound,
            PersistenceError::ConcurrentModification(_) => rocket::http::Status::Conflict,
            PersistenceError::SerializationError(_) => rocket::http::Status::InternalServerError,
            PersistenceError::DatabaseError(_) => rocket::http::Status::InternalServerError,
        }
    }
}

pub type Result<T> = std::result::Result<T, PersistenceError>;

/// Async trait for battle persistence operations
#[async_trait]
pub trait BattleRepository: Send + Sync {
    /// Create a new battle (version = 0)
    async fn create(&self, battle: &Battle) -> Result<()>;

    /// Get a battle by ID
    async fn get(&self, id: Uuid) -> Result<Battle>;

    /// Update battle (increments version, checks for conflicts)
    async fn update(&self, battle: &Battle) -> Result<()>;

    /// Delete a battle
    async fn delete(&self, id: Uuid) -> Result<()>;

    /// Health check for the repository
    async fn health_check(&self) -> Result<()>;
}

// Module declarations
pub mod postgres;
pub mod cached;
pub mod in_memory;

// Re-exports
pub use postgres::PostgresRepository;
pub use cached::CachedRepository;
pub use in_memory::InMemoryRepository;
