CREATE TABLE zot4plan_imports (
    schedule_id TEXT NOT NULL,
    user_id INTEGER NOT NULL,
    "timestamp" TIMESTAMP NOT NULL,
    PRIMARY KEY (schedule_id, user_id, "timestamp"),
    FOREIGN KEY (user_id) REFERENCES "user"(id)
)
