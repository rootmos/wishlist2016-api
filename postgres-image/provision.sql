CREATE TABLE events (id BIGSERIAL PRIMARY KEY, user_id VARCHAR, time TIMESTAMP, payload TEXT);
CREATE INDEX events_by_uid ON events (user_id);
