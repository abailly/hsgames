-- Initial schema for Pacific War battle persistence

CREATE TABLE battles (
    id UUID PRIMARY KEY,
    battle_state JSONB NOT NULL,
    version INTEGER NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Index on updated_at for potential "recent battles" queries
CREATE INDEX idx_battles_updated_at ON battles(updated_at DESC);

-- Trigger to auto-update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_battles_updated_at
    BEFORE UPDATE ON battles
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();
