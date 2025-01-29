import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';

import { useAppSelector } from '../../store/hooks';

import { SearchIndex, CourseGQLData, ProfessorGQLData } from '../../types/types';
import SearchPagination from '../SearchPagination/SearchPagination';
import noResultsImg from '../../asset/no-results-crop.webp';
import { getAllCoursesFromPlan, validateCourse } from '../../helpers/planner';
import { Spinner } from 'react-bootstrap';

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
}: Required<SearchHitContainerProps> & { results: CourseGQLData[] | ProfessorGQLData[] }) => {
  const roadmap = useAppSelector((state) => state.roadmap);
  const allExistingCourses = getAllCoursesFromPlan(roadmap?.plans[roadmap.currentPlanIndex].content);
  const transfers = roadmap?.transfers.map((transfer) => transfer.name);

  if (index === 'courses') {
    return (results as CourseGQLData[]).map((course, i) => {
      const requiredCourses = Array.from(
        validateCourse(
          new Set([...allExistingCourses, ...transfers]),
          course.prerequisiteTree,
          new Set(),
          course.corequisites,
        ),
      );
      return (
        <CourseHitItem key={course.id} index={i} {...course} {...(requiredCourses.length > 0 && { requiredCourses })} />
      );
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

  const noResults = results.length === 0 && !searchInProgress;

  return (
    <div ref={containerDivRef} className="search-hit-container">
      {noResults && (
        <div className="no-results">
          <img src={noResultsImg} alt="No results found" />
          {query === ''
            ? `Start typing in the search bar to search for ${index === 'courses' ? 'courses' : 'professors'}...`
            : "Sorry, we couldn't find any results for that search!"}
        </div>
      )}
      {searchInProgress && (
        <div className="no-results">
          <Spinner animation="border" role="status" />
        </div>
      )}
      {!searchInProgress && results.length > 0 && (
        <SearchResults
          index={index}
          results={results}
          CourseHitItem={CourseHitItem}
          ProfessorHitItem={ProfessorHitItem!}
        />
      )}
      {!searchInProgress && (
        <div className="search-pagination">
          <SearchPagination index={index} />
        </div>
      )}
    </div>
  );
};

export default SearchHitContainer;
