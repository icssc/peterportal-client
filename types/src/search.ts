import { paths } from './generated/anteater-api-types';
import { PrerequisiteTree } from './course';

type SearchAAPIResponseEntry =
  paths['/v2/rest/search']['get']['responses'][200]['content']['application/json']['data'][number];

type _SearchAAPICourseResponseEntry = Extract<SearchAAPIResponseEntry, { type: 'course' }>;

interface SearchAAPIResponseCourseEntry extends _SearchAAPICourseResponseEntry {
  prerequisiteTree: PrerequisiteTree;
}

export type SearchAAPIResponse = Array<
  Extract<SearchAAPIResponseEntry, { type: 'instructor' } | SearchAAPIResponseCourseEntry>
>;
