import { db } from '.././src/db';
import { transferredMisc } from '.././src/db/schema';
import { ANTEATER_API_REQUEST_HEADERS } from '.././src/helpers/headers';
import dotenv from 'dotenv-flow';
import { CourseAAPIResponse } from '../../types/src/course';
// load env (because this is a separate script)
dotenv.config();

// Script for organizing items from the transferredMisc table
// into transferredApExam, transferredGe, and transferredCourse
// (transfer items that could not be converted remain in transferredMisc)

// NOTE: you can use `npx tsx ./api/scripts/transfersDataConversion.ts` to run

type ApExamBasicInfo = {
  fullName: string; // E.g. "AP Microeconomics"
  catalogueName: string | undefined; // E.g. "AP ECONOMICS:MICRO"
};

const removeAllSpaces = (transferName: string | undefined) => {
  if (!transferName) return '';
  return transferName.replace(/\s/g, '');
};

/** Get all AP exams */
const getAPIApExams = async (): Promise<ApExamBasicInfo[]> => {
  const response = await fetch(`${process.env.PUBLIC_API_URL}apExams`, {
    headers: ANTEATER_API_REQUEST_HEADERS,
  })
    .then((res) => res.json())
    .then((res) => res.data as ApExamBasicInfo[]);
  return response;
};

/** Get a course based on its ID, or undefined if it does not exist */
const getAPICourseById = async (courseId: string): Promise<CourseAAPIResponse | undefined> => {
  const response = await fetch(`${process.env.PUBLIC_API_URL}courses/${encodeURIComponent(courseId)}`, {
    headers: ANTEATER_API_REQUEST_HEADERS,
  })
    .then((res) => res.json())
    .then((res) => (res.ok ? (res.data as CourseAAPIResponse) : undefined));
  return response;
};

/** Normalize a transfer name by converting it to lowercase
  then removing punctuation and trailing/leading whitespace;
  if undefined, return an empty string */
const normalizeTransferName = (transferName: string | undefined) => {
  if (!transferName) return '';
  return transferName
    .toLowerCase()
    .replace(/[.,/#!$%^&*;:{}=\-_`~()]/g, '')
    .trim();
};

/**
  Check whether a specific AP is a match for a transfer name, returning how good the match is
  0: not a match
  1: a partial match (starts with)
  2: an exact match (normalized)
*/
const apMatchQuality = (transferName: string, ap: ApExamBasicInfo): number => {
  // TODO: improve this algorithm?
  // TODO: hardcode any specific exceptions (ex. "ap us his(tory)", "ap us gov(ernment)", "ap calculus ab" with the BC subscore)?
  // TODO: handle duplicates?
  const normalizedName = normalizeTransferName(transferName);
  const normalizedApFull = normalizeTransferName(ap.fullName);
  const normalizedApCat = normalizeTransferName(ap.catalogueName);
  if (normalizedApFull == normalizedName) return 2;
  if (normalizedApCat == normalizedName) return 2;
  if (normalizedApFull.startsWith(normalizedName)) return 1;
  if (normalizedApCat.startsWith(normalizedName)) return 1;
  return 0;
};

/** Try to find the best match for a transfer name out of all the AP exams; undefined if no match */
const tryMatchAp = (transferName: string, allAps: ApExamBasicInfo[]): ApExamBasicInfo | undefined => {
  // TODO: test
  let bestMatch: ApExamBasicInfo | undefined = undefined;
  let bestMatchQuality = 0;
  for (const ap of allAps) {
    const matchQuality = apMatchQuality(transferName, ap);
    if (matchQuality > bestMatchQuality) {
      bestMatch = ap;
      bestMatchQuality = matchQuality;
    }
  }
  return bestMatch;
};

/** Try to match a transfer name with an existing UCI course; undefined if no match */
const tryMatchCourse = async (transferName: string): Promise<string | undefined> => {
  // TODO: improve comparison with spaces?
  const res = await getAPICourseById(removeAllSpaces(transferName));
  if (!res) {
    return undefined;
  } else {
    // Matched properly, but the resulting ID will not have proper spaces, so return original
    return transferName;
  }
};

/** Organize the data in the database */
const organize = async () => {
  const allAps = await getAPIApExams();
  console.log('DEBUG: ALL APS: ');
  for (const ap of allAps) {
    console.log(`  - '${ap.fullName}' (cat: '${ap.catalogueName}')`);
  }
  const transfers = await db
    .select({
      userId: transferredMisc.userId,
      courseName: transferredMisc.courseName,
      units: transferredMisc.units,
    })
    .from(transferredMisc)
    .limit(10); // For testing, we will only look at a few entries
  for (const transfer of transfers) {
    if (transfer.courseName == null || transfer.userId == null) {
      // TODO: should we just remove it from the database in this case, because it's clutter?
      continue;
    }
    const transferName = transfer.courseName.trim();
    if (transferName.startsWith('AP ')) {
      const bestMatch = tryMatchAp(transferName, allAps);
      if (bestMatch) {
        // Move this transfer item to the APs table with the name of the best match
        /*const newApRow = {
          userId: transfer.userId,
          examName: bestMatch.catalogueName ?? transferName, // TODO: should we do this when catalogueName doesn't exist?
          score: null,
          units: transfer.units ?? 4, // TODO: should we default to 4?
        };*/
        console.log(`  MATCHED: '${transferName}' with: '${bestMatch.fullName}' ('${bestMatch.catalogueName}')`);
      } else {
        // Could not match; leave it here
        console.log(`x FAILED:  '${transferName}'      could not be matched with any AP`);
      }
    } else {
      const bestMatch = await tryMatchCourse(transferName);
      if (bestMatch) {
        // Move this transfer item to the transferred courses table with the name of the best match
        /*const newCourseRow = {
          userId: transfer.userId,
          courseName: bestMatch,
          units: transfer.units ?? 0
        };*/
        console.log(`  MATCHED: '${transferName}' with: '${bestMatch}'`);
      } else {
        // Could not match; leave it here
        console.log(`x FAILED:  '${transferName}'      could not be matched with any course`);
      }
    }
  }
};

organize();
