/**
 @module ProgramsRoute
*/

import { publicProcedure, router } from '../helpers/trpc';
import { MajorProgram, MajorSpecialization, MinorProgram, ProgramRequirement } from '@peterportal/types';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';
import { z } from 'zod';
import { db } from '../db';
import { planner, plannerMajor } from '../db/schema';
import { and, eq } from 'drizzle-orm';

type ProgramType = MajorProgram | MinorProgram | MajorSpecialization;
const programTypeNames = ['major', 'minor', 'specialization'] as const;
const ugradRequirementTypeNames = ['UC', 'GE'] as const;

const getAPIProgramData = async <T extends ProgramType>(programType: string): Promise<T[]> => {
  const response = await fetch(`${process.env.PUBLIC_API_URL}programs/${programType}`, {
    headers: ANTEATER_API_REQUEST_HEADERS,
  })
    .then((res) => res.json())
    .then((res) => res.data as T[]);
  return response;
};

const zodMajorSpecPairSchema = z.object({
  plannerId: z.number(),
  pairs: z.array(
    z.object({
      majorId: z.string(),
      specializationId: z.string().optional(),
    }),
  ),
});

const programsRouter = router({
  getMajors: publicProcedure.query(async () => {
    return getAPIProgramData<MajorProgram>('majors').then((r) => r.filter((m) => m.division === 'Undergraduate'));
  }),
  getMinors: publicProcedure.query(async () => {
    return getAPIProgramData<MinorProgram>('minors');
  }),
  getSpecializations: publicProcedure.input(z.object({ major: z.string() })).query(async ({ input }) => {
    const url = `${process.env.PUBLIC_API_URL}programs/specializations?majorId=${input.major}`;
    const response = await fetch(url, { headers: ANTEATER_API_REQUEST_HEADERS })
      .then((res) => res.json())
      .then((res) => res.data as MajorSpecialization[]);
    return response;
  }),
  getRequiredCourses: publicProcedure
    .input(z.object({ type: z.enum(programTypeNames), programId: z.string() }))
    .query(async ({ input }) => {
      const url = `${process.env.PUBLIC_API_URL}programs/${input.type}?programId=${input.programId}`;
      const response = await fetch(url, { headers: ANTEATER_API_REQUEST_HEADERS })
        .then((res) => res.json())
        .then((res) => res.data.requirements as ProgramRequirement[]);
      return response;
    }),
  getRequiredCoursesUgrad: publicProcedure
    .input(z.object({ id: z.enum(ugradRequirementTypeNames) }))
    .query(async ({ input }) => {
      const url = `${process.env.PUBLIC_API_URL}programs/ugradRequirements?id=${input.id}`;
      const response = await fetch(url, { headers: ANTEATER_API_REQUEST_HEADERS })
        .then((res) => res.json())
        .then((res) => res.data.requirements as ProgramRequirement[]);
      return response;
    }),
  getSavedMajorSpecPairs: publicProcedure.input(z.number()).query(async ({ input: plannerId, ctx }) => {
    const userId = ctx.session.userId;
    if (!userId) return [];

    return await db
      .select({ majorId: plannerMajor.majorId, specializationId: plannerMajor.specializationId })
      .from(plannerMajor)
      .innerJoin(planner, eq(planner.id, plannerMajor.plannerId))
      .where(and(eq(plannerMajor.plannerId, plannerId), eq(planner.userId, userId)));
  }),
  /** @todo when allowing multiple majors, we should instead have operations to add/remove a pair (for add/remove major) and update pair (change major spec) */
  saveSelectedMajorSpecPair: publicProcedure.input(zodMajorSpecPairSchema).mutation(async ({ input }) => {
    const { plannerId, pairs } = input;
    await db.delete(plannerMajor).where(eq(plannerMajor.plannerId, plannerId));

    const rowsToInsert = pairs.map((p) => ({ plannerId, majorId: p.majorId, specializationId: p.specializationId }));
    await db.insert(plannerMajor).values(rowsToInsert);
  }),
  /** @todo add `setPlannerMinor` (or similarly named) operation for updating a minor */
});

export default programsRouter;
