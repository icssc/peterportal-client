import { SavedPlannerYearData } from '@peterportal/types';
import { sql } from 'drizzle-orm';
import {
  boolean,
  check,
  index,
  integer,
  jsonb,
  pgTable,
  primaryKey,
  real,
  text,
  timestamp,
  unique,
} from 'drizzle-orm/pg-core';

export const user = pgTable(
  'user',
  {
    id: integer().primaryKey().generatedAlwaysAsIdentity(),
    googleId: text().notNull(),
    name: text().notNull(),
    email: text().notNull(),
    picture: text().notNull(),
    theme: text(),
    lastRoadmapEditAt: timestamp(),
  },
  (table) => ({
    uniqueGoogleId: unique('unique_google_id').on(table.googleId),
  }),
);

export const report = pgTable('report', {
  id: integer().primaryKey().generatedAlwaysAsIdentity(),
  reviewId: integer()
    .notNull()
    .references(() => review.id, { onDelete: 'cascade' }),
  reason: text().notNull(),
  createdAt: timestamp().defaultNow().notNull(),
});

export const review = pgTable(
  'review',
  {
    id: integer().primaryKey().generatedAlwaysAsIdentity(),
    professorId: text().notNull(),
    courseId: text().notNull(),
    userId: integer()
      .notNull()
      .references(() => user.id),
    anonymous: boolean().notNull(),
    content: text(),
    rating: integer().notNull(),
    difficulty: integer().notNull(),
    gradeReceived: text().notNull(),
    createdAt: timestamp().defaultNow().notNull(),
    updatedAt: timestamp().defaultNow(),
    forCredit: boolean().notNull(),
    quarter: text().notNull(),
    // score: integer().notNull() /** @todo: could do a count query on votes instead? */,
    takeAgain: boolean().notNull(),
    textbook: boolean().notNull(),
    attendance: boolean().notNull(),
    tags: text().array(),
    verified: boolean().notNull().default(false),
  },
  (table) => ({
    ratingCheck: check('rating_check', sql`${table.rating} >= 1 AND ${table.rating} <= 5`),
    difficultyCheck: check('difficulty_check', sql`${table.difficulty} >= 1 AND ${table.difficulty} <= 5`),
    unique: unique('unique_review').on(table.userId, table.professorId, table.courseId),
    professorIdIdx: index('reviews_professor_id_idx').on(table.professorId),
    courseIdIdx: index('reviews_course_id_idx').on(table.courseId),
  }),
);

export const planner = pgTable(
  'planner',
  {
    id: integer().primaryKey().generatedAlwaysAsIdentity(),
    userId: integer().references(() => user.id),
    name: text().notNull(),
    years: jsonb().$type<SavedPlannerYearData>().array().notNull(),
  },
  (table) => ({
    userIdIdx: index('planners_user_id_idx').on(table.userId),
  }),
);

export const transferredCourse = pgTable(
  'transferred_course',
  {
    userId: integer().references(() => user.id),
    courseName: text(),
    units: real(),
  },
  (table) => ({
    userIdIdx: index('transferred_courses_user_id_idx').on(table.userId),
  }),
);

export const vote = pgTable(
  'vote',
  {
    reviewId: integer()
      .notNull()
      .references(() => review.id, { onDelete: 'cascade' }),
    userId: integer()
      .notNull()
      .references(() => user.id),
    vote: integer().notNull(),
  },
  (table) => ({
    voteCheck: check('votes_vote_check', sql`${table.vote} = 1 OR ${table.vote} = -1`),
    // reviewId first since we make 2 kinds of queries: on just reviewId and on reviewId, userId
    primaryKey: primaryKey({ columns: [table.reviewId, table.userId] }),
  }),
);

export const savedCourse = pgTable(
  'saved_course',
  {
    userId: integer()
      .references(() => user.id)
      .notNull(),
    courseId: text().notNull(),
  },
  (table) => ({
    primaryKey: primaryKey({ columns: [table.userId, table.courseId] }),
  }),
);

export const session = pgTable('session', {
  sid: text().primaryKey(),
  sess: jsonb().notNull(),
  expire: timestamp().notNull(),
});
