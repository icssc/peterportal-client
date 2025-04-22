import { db } from '.././src/db';
import { transferredMisc } from '.././src/db/schema';
import { ANTEATER_API_REQUEST_HEADERS } from '.././src/helpers/headers';

// Script for organizing items from the transferredMisc table
// into transferredApExam, transferredGe, and transferredCourse
// (transfer items that could not be converted remain in transferredMisc)

type ApExamBasicInfo = {
  fullName: string; // E.g. "AP Microeconomics"
  catalogueName: string; // E.g. "AP ECONOMICS:MICRO"
};

/** Normalize a transfer name by converting it to lowercase
  then removing punctuation and trailing/leading whitespace */
const normalizeTransferName = (transferName: string) => {
  return transferName
    .toLowerCase()
    .replace(/[.,/#!$%^&*;:{}=\-_`~()]/g, '')
    .trim();
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

/** Get whether a course exists */
/*const getCourseMatch = async (transferName): Promise<boolean> => {
  // TODO: implement
  return false;
}*/

/** Get whether an AP is a match for a transfer name */
const isApMatch = (normalizedName: string, ap: ApExamBasicInfo): boolean => {
  if (normalizeTransferName(ap.fullName) == normalizedName) return true;
  if (normalizeTransferName(ap.catalogueName) == normalizedName) return true;
  if (normalizeTransferName(ap.fullName).startsWith(normalizedName)) return true;
  if (normalizeTransferName(ap.catalogueName).startsWith(normalizedName)) return true;
  return false;
};

/** Organize the data in the database */
const organize = async () => {
  const allAps = await getAPIApExams();
  const transfers = await db
    .select({
      userId: transferredMisc.userId,
      courseName: transferredMisc.courseName,
      units: transferredMisc.units,
    })
    .from(transferredMisc)
    .limit(5); // For testing, we will only look at a few entries
  for (const transfer of transfers) {
    const normalizedName = normalizeTransferName(transfer.courseName ?? '');
    if (normalizedName.startsWith('AP ')) {
      // AP -> search in catalogue and full names
      let bestMatch: ApExamBasicInfo | undefined = undefined;
      for (const ap of allAps) {
        if (isApMatch(normalizedName, ap)) {
          bestMatch = ap;
        }
      }
      if (bestMatch) {
        // Move this transfer item to the APs table with the name of the best match
      }
    } else {
      // Search for a course
      /*if (getCourseMatch(*/
      // If course search failed,
    }
  }
};

organize();
