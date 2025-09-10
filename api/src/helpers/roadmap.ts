import { asc, eq, SQL, sql } from 'drizzle-orm';
import { db } from '../db';
import { planner, plannerQuarter, plannerYear, user } from '../db/schema';
import { getTableConfig } from 'drizzle-orm/pg-core';
import { quarters, SavedPlannerData } from '@peterportal/types';

export async function queryGetPlanners(where: SQL) {
  const planYearTableName = getTableConfig(plannerYear).name;
  const planners = await db
    .select({
      id: planner.id,
      name: planner.name,
      content: sql.raw(`jsonb_agg(jsonb_build_object(
      'name', ${planYearTableName}."${plannerYear.name.name}",
      'startYear', ${planYearTableName}."${plannerYear.startYear.name}",
      'quarters', (SELECT jsonb_agg(jsonb_build_object(
        'name', ${plannerQuarter.quarterName.name},
        'courses', (
          SELECT COALESCE(jsonb_agg(pc.course_id), '[]'::jsonb)
          FROM planner_course pc
          WHERE pc.planner_id = pq.planner_id
            AND pc.start_year = pq.start_year
            AND pc.quarter_name = pq.quarter_name
        )
      )) AS quarters FROM planner_quarter pq
        WHERE pq.planner_id = ${planYearTableName}."${plannerYear.plannerId.name}"
          AND pq.start_year = ${planYearTableName}."${plannerYear.startYear.name}")
    ))`),
    })
    .from(planner)
    .innerJoin(plannerYear, eq(planner.id, plannerYear.plannerId))
    .innerJoin(user, eq(planner.userId, user.id))
    .where(where)
    .groupBy(planner.id, planner.name)
    .orderBy(asc(planner.id));

  (planners as SavedPlannerData[]).forEach((planner) =>
    planner.content.forEach((year) => {
      year.quarters.sort((a, b) => quarters.indexOf(a.name) - quarters.indexOf(b.name));
    }),
  );

  return planners;
}
