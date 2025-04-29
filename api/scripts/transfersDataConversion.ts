import { db } from '.././src/db';
import { transferredMisc } from '.././src/db/schema';
import { ANTEATER_API_REQUEST_HEADERS } from '.././src/helpers/headers';
import dotenv from 'dotenv-flow';
import { count, and, eq, SQL, sql } from 'drizzle-orm';
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
    .replace(/[.,/#!$%^*;:{}=\-_`~()]/g, '')
    .replace('&', 'and')
    .trim();
};

/**
  Substitute words from a normalized transfer name to look closer to an AP exam
  Note: order DOES matter
*/
const apSubstitutions = (normalizedName: string, substitutions: { from: string; to: string }[]) => {
  let res = normalizedName;
  for (const sub of substitutions) {
    const re = new RegExp(String.raw`(^|\s+)(${sub.from})(\s+|$)`);
    res = res.replace(re, `$1${sub.to}$3`);
  }
  return res;
};

/**
  Check whether a specific AP is a match for a transfer name, returning how good the match is
  0: not a match
  1: a partial match/starts with (with substitutions)
  2: an exact match (with substitutions)
  3: a partial match/starts with (no substitutions)
  4: an exact match (no substitutions)
*/
const apMatchQuality = (normalizedName: string, comparingName: string, ap: ApExamBasicInfo): number => {
  // TODO: improve this algorithm?
  // TODO: hardcode any specific exceptions (ex. "ap us his(tory)", "ap us gov(ernment)", "ap calculus ab" with the BC subscore)?
  //console.log(comparingName);
  const normalizedApFull = normalizeTransferName(ap.fullName);
  const normalizedApCat = normalizeTransferName(ap.catalogueName);
  if (normalizedApFull == normalizedName) return 4;
  if (normalizedApCat == normalizedName) return 4;
  if (normalizedApFull.startsWith(normalizedName)) return 3;
  if (normalizedApCat.startsWith(normalizedName)) return 3;
  if (normalizedApFull == comparingName) return 2;
  if (normalizedApCat == comparingName) return 2;
  if (normalizedApFull.startsWith(comparingName)) return 1;
  if (normalizedApCat.startsWith(comparingName)) return 1;
  return 0;
};

/** Try to find the best match for a transfer name out of all the AP exams; undefined if no match */
const tryMatchAp = (transferName: string, allAps: ApExamBasicInfo[]): ApExamBasicInfo | undefined => {
  // Normalize the transfer name
  const normalizedName = normalizeTransferName(transferName);
  // Some hardcoded exceptions (specifically for matching the full name rather than cat name)
  const comparingName = apSubstitutions(normalizedName, [
    { from: 'us', to: 'united states' },
    { from: 'lang', to: 'language' },
    { from: 'lng', to: 'language' },
    { from: 'lit', to: 'literature' },
    { from: 'hist', to: 'history' },
    { from: 'gov', to: 'government' },
    { from: 'govt', to: 'government' },
    { from: 'stats', to: 'statistics' },
    { from: 'calc', to: 'calculus' },
    { from: 'comp', to: 'computer' },
    { from: 'sci', to: 'science' },
    { from: 'eng', to: 'english' },
    { from: 'spa', to: 'spanish' },
    { from: 'ap literature', to: 'ap english literature' },
    { from: 'ap language', to: 'ap english language' },
    { from: 'ap government', to: 'ap united states government' },
    //{ from: "&", to: "and" },
  ]);
  let bestMatch: ApExamBasicInfo | undefined = undefined;
  let bestMatchQuality = 0;
  for (const ap of allAps) {
    const matchQuality = apMatchQuality(normalizedName, comparingName, ap);
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
  // TODO: Do grouping and store a count
  // Either deleting none or deleting all
  const transfers = await db
    .select({
      userId: transferredMisc.userId,
      courseName: transferredMisc.courseName,
      //units: transferredMisc.units,
      totalUnits: sql<number>`coalesce(sum(${transferredMisc.units}), 0)`.mapWith(Number),
      count: count(transferredMisc.userId),
    })
    .from(transferredMisc)
    .groupBy(transferredMisc.userId, transferredMisc.courseName)
    .limit(10); // For testing, we will only look at a few entries*/
  const toDelete: (SQL<unknown> | undefined)[] = [];
  for (const transfer of transfers) {
    if (transfer.courseName == null || transfer.userId == null || transfer.count == 0) {
      // TODO: should we just remove it from the database in this case, because it's clutter?
      continue;
    }
    const transferName = transfer.courseName.trim();
    if (transferName.startsWith('AP ')) {
      const bestMatch = tryMatchAp(transferName, allAps);
      if (bestMatch) {
        // Move this transfer item to the APs table with the name of the best match
        // TODO: move (consider how to handle duplicates)
        // TODO: if duplicates found, don't duplicate it
        // If different units among the duplicates, get all the units then divide evenly among the two/several
        /*const newApRow = {
          userId: transfer.userId,
          examName: bestMatch.fullName,
          score: null,
          // Divide by count to get average units among duplicates (we know count is nonzero, checked above)
          units: transfer.totalUnits / transfer.count,
        };*/
        toDelete.push(
          and(
            eq(transferredMisc.userId, transfer.userId),
            eq(transferredMisc.courseName, transfer.courseName),
            //transfer.units ? eq(transferredMisc.units, transfer.units) : undefined
          ),
        );
        console.log(`  MATCHED: '${transferName}' with: '${bestMatch.fullName}' ('${bestMatch.catalogueName}')`);
        // TODO: Delete with or (if desired)
      } else {
        // Could not match; leave it here
        console.log(`x FAILED:  '${transferName}'      could not be matched with any AP`);
      }
    } else {
      const bestMatch = await tryMatchCourse(transferName);
      if (bestMatch) {
        // Move this transfer item to the transferred courses table with the name of the best match
        // TODO: move (consider how to handle duplicates)
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
  // Delete everything with the query
  // TODO: print/delete everything that should be deleted
};

organize();
