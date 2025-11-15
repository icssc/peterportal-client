/**
 @module DepartmentRoute
*/
import { publicProcedure, router } from '../helpers/trpc';
import { DepartmentsAAPIResponse } from '@peterportal/types';
import { ANTEATER_API_REQUEST_HEADERS } from '../helpers/headers';

const departmentRouter = router({
  get: publicProcedure.query(async () => {
    const r = fetch(`${process.env.PUBLIC_API_URL}websoc/departments`, {
      headers: ANTEATER_API_REQUEST_HEADERS,
    });

    return r.then((response) => response.json()).then((data) => data.data as DepartmentsAAPIResponse);
  }),
});

export default departmentRouter;
