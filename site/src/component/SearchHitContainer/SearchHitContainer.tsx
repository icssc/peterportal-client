import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';

import { useAppSelector } from '../../store/hooks';

import { SearchIndex, CourseGQLData, ProfessorGQLData } from '../../types/types';
import SearchPagination from '../SearchPagination/SearchPagination';

interface SearchHitContainerProps {
  index: SearchIndex;
  CourseHitItem: FC<CourseGQLData & { index: number }>;
  ProfessorHitItem?: FC<ProfessorGQLData & { index: number }>;
}

const SearchHitContainer: FC<SearchHitContainerProps> = ({ index, CourseHitItem, ProfessorHitItem }) => {
  const courseResults = useAppSelector((state) => state.search.courses.results);
  const professorResults = useAppSelector((state) => state.search.professors.results);
  const containerDivRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    containerDivRef.current!.scrollTop = 0;
  }, [courseResults, professorResults]);

  if (index == 'professors' && !ProfessorHitItem) {
    throw 'Professor Component not provided';
  }

  return (
    <div ref={containerDivRef} className="search-hit-container">
      {index == 'courses' && (
        <>
          {courseResults.length === 0 ? (
            <div className="no-results">No results found</div>
          ) : (
            courseResults.map((course, i) => {
              return <CourseHitItem key={`course-hit-item-${i}`} index={i} {...(course as CourseGQLData)} />;
            })
          )}
        </>
      )}
      {index == 'professors' && ProfessorHitItem && (
        <>
          {professorResults.length === 0 ? (
            <div className="no-results">No results found</div>
          ) : (
            professorResults.map((professor, i) => {
              return (
                <ProfessorHitItem key={`professor-hit-item-${i}`} index={i} {...(professor as ProfessorGQLData)} />
              );
            })
          )}
        </>
      )}
      <div className="search-pagination">
        <SearchPagination index={index} />
      </div>
    </div>
  );
};

export default SearchHitContainer;
