/**
 @module ProgramsRoute
*/

import { publicProcedure, router } from '../helpers/trpc';
import { MajorProgram, MajorSpecialization, MinorProgram, ProgramRequirement } from '@peterportal/types';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';
import { z } from 'zod';

type ProgramType = MajorProgram | MinorProgram | MajorSpecialization;
const programTypeNames = ['major', 'minor', 'specialization'] as const;

const getAPIProgramData = async <T extends ProgramType>(programType: string): Promise<T[]> => {
  const response = await fetch(`${process.env.PUBLIC_API_URL}programs/${programType}`, {
    headers: ANTEATER_API_REQUEST_HEADERS,
  })
    .then((res) => res.json())
    .then((res) => res.data as T[]);
  return response;
};

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
});

export default programsRouter;
