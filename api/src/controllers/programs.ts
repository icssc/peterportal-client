/**
 @module ProgramsRoute
*/

import { publicProcedure, router } from '../helpers/trpc';
import {
  MajorProgram,
  MajorSpecialization,
  MajorSpecializationPair,
  MinorProgram,
  ProgramRequirement,
  SavedMinorProgram,
} from '@peterportal/types';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';
import { z } from 'zod';
import { db } from '../db';
import { planner, userMajor, userMajorCatalogYear, userMinor, userMinorCatalogYear } from '../db/schema';
import { and, eq } from 'drizzle-orm';

type ProgramType = MajorProgram | MinorProgram | MajorSpecialization;
const programTypeNames = ['major', 'minor', 'specialization'] as const;
const ugradRequirementTypeNames = ['UC', 'GE', 'CHC4', 'CHC2'] as const;

const getAPIProgramData = async <T extends ProgramType>(programType: string): Promise<T[]> => {
  const response = await fetch(`${process.env.PUBLIC_API_URL}programs/${programType}`, {
    headers: ANTEATER_API_REQUEST_HEADERS,
  })
    .then((res) => res.json())
    .then((res) => res.data as T[]);
  return response;
};

const zodMajorSpecPairSchema = z.object({
  pairs: z.array(
    z.object({
      majorId: z.string(),
      specializationId: z.string().optional(),
      catalogYear: z.number().int().nullable().optional(),
    }),
  ),
});

const zodMinorProgramSchema = z.object({
  minorIds: z.array(z.string()),
  catalogYears: z
    .array(
      z.object({
        minorId: z.string(),
        catalogYear: z.number().int().nullable().optional(),
      }),
    )
    .optional(),
});

function dedupeCatalogYears<T extends { catalogYear?: number | null }>(
  rows: T[],
  getId: (row: T) => string,
): (T & { catalogYear: number })[] {
  const byId = new Map<string, T & { catalogYear: number }>();
  rows.forEach((row) => {
    if (row.catalogYear == null) return;
    byId.set(getId(row), { ...row, catalogYear: row.catalogYear });
  });
  return [...byId.values()];
}

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
    .input(z.object({ type: z.enum(programTypeNames), programId: z.string(), specializationId: z.string().optional() }))
    .query(async ({ input }) => {
      let url = `${process.env.PUBLIC_API_URL}programs/${input.type}?programId=${input.programId}`;
      if (input.type === 'major' && input.specializationId) {
        url += `&specializationId=${input.specializationId}`;
      }
      const response = await fetch(url, { headers: ANTEATER_API_REQUEST_HEADERS })
        .then((res) => res.json())
        .then((res) => {
          const schoolRequirements = (res.data.schoolRequirements?.requirements as ProgramRequirement[]) ?? [];
          const majorRequirements = res.data.requirements as ProgramRequirement[];
          return [...schoolRequirements, ...majorRequirements];
        });
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
  getSavedMajorSpecPairs: publicProcedure.query(async ({ ctx }): Promise<MajorSpecializationPair[]> => {
    const userId = ctx.session.userId;
    if (!userId) return [];

    const pairs = await db
      .select({
        majorId: userMajor.majorId,
        specializationId: userMajor.specializationId,
        catalogYear: userMajorCatalogYear.catalogYear,
      })
      .from(userMajor)
      .leftJoin(
        userMajorCatalogYear,
        and(eq(userMajorCatalogYear.userId, userMajor.userId), eq(userMajorCatalogYear.majorId, userMajor.majorId)),
      )
      .where(eq(userMajor.userId, userId));

    const res = pairs.map((p) => ({
      majorId: p.majorId,
      specializationId: p.specializationId ?? undefined,
      catalogYear: p.catalogYear ?? undefined,
    }));

    return res;
  }),
  getSavedMinors: publicProcedure.query(async ({ ctx }): Promise<SavedMinorProgram[]> => {
    const userId = ctx.session.userId;
    if (!userId) return [];

    const res = await db
      .select({ minorId: userMinor.minorId, catalogYear: userMinorCatalogYear.catalogYear })
      .from(userMinor)
      .leftJoin(
        userMinorCatalogYear,
        and(eq(userMinorCatalogYear.userId, userMinor.userId), eq(userMinorCatalogYear.minorId, userMinor.minorId)),
      )
      .where(eq(userMinor.userId, userId));

    return res.map((r) => ({ id: r.minorId, name: '', catalogYear: r.catalogYear ?? undefined }));
  }),
  /** @todo when allowing multiple majors, we should instead have operations to add/remove a pair (for add/remove major) and update pair (change major spec) */
  saveSelectedMajorSpecPair: publicProcedure.input(zodMajorSpecPairSchema).mutation(async ({ input, ctx }) => {
    const userId = ctx.session.userId;
    if (!userId) throw new Error('Unauthorized');

    const { pairs } = input;

    const rowsToInsert = pairs.map((p) => ({
      userId,
      majorId: p.majorId,
      specializationId: p.specializationId,
    }));
    const catalogYearRowsToInsert = dedupeCatalogYears(pairs, (pair) => pair.majorId).map((p) => ({
      userId,
      majorId: p.majorId,
      catalogYear: p.catalogYear,
    }));

    await db.transaction(async (tx) => {
      await tx.delete(userMajor).where(eq(userMajor.userId, userId));
      if (rowsToInsert.length) {
        await tx.insert(userMajor).values(rowsToInsert);
      }
      if (catalogYearRowsToInsert.length) {
        await tx.insert(userMajorCatalogYear).values(catalogYearRowsToInsert);
      }
    });
  }),
  /** @todo add `setPlannerMinor` (or similarly named) operation for updating a minor */
  saveSelectedMinor: publicProcedure.input(zodMinorProgramSchema).mutation(async ({ input, ctx }) => {
    const userId = ctx.session.userId;
    if (!userId) throw new Error('Unauthorized');

    const { minorIds, catalogYears = [] } = input;

    const rowsToInsert = minorIds.map((minorId) => ({ userId, minorId }));
    const minorIdsToInsert = new Set(minorIds);
    const catalogYearRowsToInsert = dedupeCatalogYears(catalogYears, (row) => row.minorId)
      .filter((row) => minorIdsToInsert.has(row.minorId))
      .map((row) => ({ userId, minorId: row.minorId, catalogYear: row.catalogYear }));

    await db.transaction(async (tx) => {
      await tx.delete(userMinor).where(eq(userMinor.userId, userId));
      if (rowsToInsert.length) {
        await tx.insert(userMinor).values(rowsToInsert);
      }
      if (catalogYearRowsToInsert.length) {
        await tx.insert(userMinorCatalogYear).values(catalogYearRowsToInsert);
      }
    });
  }),
  saveCHCSelection: publicProcedure
    .input(z.object({ plannerId: z.number(), chc: z.enum(['', 'CHC4', 'CHC2']) }))
    .mutation(async ({ input }) => {
      const { plannerId, chc } = input;
      await db
        .update(planner)
        .set({ chc: chc || null })
        .where(eq(planner.id, plannerId));
    }),
});

export default programsRouter;
