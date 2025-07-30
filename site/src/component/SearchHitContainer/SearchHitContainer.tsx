import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';

import { useAppSelector } from '../../store/hooks';

import { SearchIndex, CourseGQLData, ProfessorGQLData, SearchResultData } from '../../types/types';
import SearchPagination from '../SearchPagination/SearchPagination';
import NoResults from '../NoResults/NoResults';
import { getMissingPrerequisites } from '../../helpers/planner';
import LoadingSpinner from '../LoadingSpinner/LoadingSpinner';
import { useClearedCourses } from '../../hooks/planner';

// TODO: CourseHitItem and ProfessorHitem should not need index
// investigate: see if you can refactor respective components to use course id/ucinetid for keys instead then remove index from props
interface SearchHitContainerProps {
  index: SearchIndex;
  CourseHitItem: FC<CourseGQLData & { index: number; requiredCourses?: string[] }>;
  ProfessorHitItem?: FC<ProfessorGQLData & { index: number }>;
}

const SearchResults = ({
  index,
  results,
  CourseHitItem,
  ProfessorHitItem,
}: Required<SearchHitContainerProps> & { results: SearchResultData }) => {
  const clearedCourses = useClearedCourses();

  if (index === 'courses') {
    return (results as CourseGQLData[]).map((course, i) => {
      const requiredCourses = getMissingPrerequisites(clearedCourses, course);
      return <CourseHitItem key={course.id} index={i} {...course} requiredCourses={requiredCourses} />;
    });
  } else {
    return (results as ProfessorGQLData[]).map((professor, i) => (
      <ProfessorHitItem key={professor.ucinetid} index={i} {...professor} />
    ));
  }
};

const SearchHitContainer: FC<SearchHitContainerProps> = ({ index, CourseHitItem, ProfessorHitItem }) => {
  const { query, results, searchInProgress } = useAppSelector((state) => state.search[index]);
  const containerDivRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    containerDivRef.current!.scrollTop = 0;
  }, [results]);

  if (index == 'professors' && !ProfessorHitItem) {
    throw 'Professor Component not provided';
  }

  return (
    <div ref={containerDivRef} className="search-hit-container">
      {searchInProgress && <LoadingSpinner />}
      {!searchInProgress && results.length === 0 && (
        <NoResults showPrompt={query === ''} prompt={`Start typing in the search bar to search for ${index}...`} />
      )}
      {!searchInProgress && results.length > 0 && (
        <>
          <SearchResults
            index={index}
            results={results}
            CourseHitItem={CourseHitItem}
            ProfessorHitItem={ProfessorHitItem!}
          />
          <div className="search-pagination">
            <SearchPagination index={index} />
          </div>
        </>
      )}
    </div>
  );
};

export default SearchHitContainer;
