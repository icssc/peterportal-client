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

export const users = pgTable(
  'users',
  {
    id: integer().primaryKey().generatedAlwaysAsIdentity(),
    googleId: text().notNull(),
    displayName: text().notNull(),
    email: text().notNull(),
    picture: text().notNull(),
    theme: text(),
    lastRoadmapEditAt: timestamp(),
  },
  (table) => ({
    uniqueGoogleId: unique('unique_google_id').on(table.googleId),
  }),
);

export const reports = pgTable('reports', {
  id: integer().primaryKey().generatedAlwaysAsIdentity(),
  reviewId: integer()
    .notNull()
    .references(() => reviews.id, { onDelete: 'cascade' }),
  reason: text().notNull(),
  createdAt: timestamp().defaultNow().notNull(),
});

export const reviews = pgTable(
  'reviews',
  {
    id: integer().primaryKey().generatedAlwaysAsIdentity(),
    professorId: text().notNull(),
    courseId: text().notNull(),
    userId: integer()
      .notNull()
      .references(() => users.id),
    anonymous: boolean().notNull(),
    content: text(),
    rating: integer().notNull(),
    difficulty: integer().notNull(),
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

export const planners = pgTable(
  'planners',
  {
    id: integer().primaryKey().generatedAlwaysAsIdentity(),
    userId: integer().references(() => users.id),
    name: text().notNull(),
    years: jsonb().$type<SavedPlannerYearData>().array().notNull(),
  },
  (table) => ({
    userIdIdx: index('planners_user_id_idx').on(table.userId),
  }),
);

export const transferredCourses = pgTable(
  'transferred_courses',
  {
    userId: integer().references(() => users.id),
    courseName: text(),
    units: real(),
  },
  (table) => ({
    userIdIdx: index('transferred_courses_user_id_idx').on(table.userId),
  }),
);

export const votes = pgTable(
  'votes',
  {
    reviewId: integer()
      .notNull()
      .references(() => reviews.id, { onDelete: 'cascade' }),
    userId: integer()
      .notNull()
      .references(() => users.id),
    vote: integer().notNull(),
  },
  (table) => ({
    voteCheck: check('votes_vote_check', sql`${table.vote} = 1 OR ${table.vote} = -1`),
    // reviewId first since we make 2 kinds of queries: on just reviewId and on reviewId, userId
    primaryKey: primaryKey({ columns: [table.reviewId, table.userId] }),
  }),
);

export const savedCourses = pgTable(
  'saved_courses',
  {
    userId: integer().references(() => users.id),
    courseId: text(),
  },
  (table) => ({
    primaryKey: primaryKey({ columns: [table.userId, table.courseId] }),
  }),
);
