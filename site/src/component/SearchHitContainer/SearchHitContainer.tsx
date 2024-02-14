import { useEffect, FC, useRef } from 'react';
import './SearchHitContainer.scss';

import { useAppSelector } from '../../store/hooks';

import { SearchIndex, CourseGQLData, ProfessorGQLData } from '../../types/types';
import SearchPagination from '../SearchPagination/SearchPagination';
import noResultsImg from '../../asset/no-results-crop.webp';
import { useFirstRender } from '../../hooks/firstRenderer';

// TODO: CourseHitItem and ProfessorHitem should not need index
// investigate: see if you can refactor respective components to use course id/ucinetid for keys instead then remove index from props
interface SearchHitContainerProps {
  index: SearchIndex;
  CourseHitItem: FC<CourseGQLData & { index: number }>;
  ProfessorHitItem?: FC<ProfessorGQLData & { index: number }>;
}

const SearchResults = ({
  index,
  results,
  CourseHitItem,
  ProfessorHitItem,
}: Required<SearchHitContainerProps> & { results: CourseGQLData[] | ProfessorGQLData[] }) => {
  if (index === 'courses') {
    return (results as CourseGQLData[]).map((course, i) => <CourseHitItem key={course.id} index={i} {...course} />);
  } else {
    return (results as ProfessorGQLData[]).map((professor, i) => (
      <ProfessorHitItem key={professor.ucinetid} index={i} {...professor} />
    ));
  }
};

const SearchHitContainer: FC<SearchHitContainerProps> = ({ index, CourseHitItem, ProfessorHitItem }) => {
  const { names, results } = useAppSelector((state) => state.search[index]);
  const containerDivRef = useRef<HTMLDivElement>(null);
  const isFirstRender = useFirstRender();

  useEffect(() => {
    containerDivRef.current!.scrollTop = 0;
  }, [results]);

  if (index == 'professors' && !ProfessorHitItem) {
    throw 'Professor Component not provided';
  }

  /**
   * if its first render, we are waiting for initial results
   * if names is non-empty but results is empty, we are waiting for results
   * otherwise, if results is still empty, we have no results for the search
   */
  const noResults = results.length === 0 && !(isFirstRender || names.length > 0);

  return (
    <div ref={containerDivRef} className="search-hit-container">
      {noResults && (
        <div className="no-results">
          <img src={noResultsImg} alt="No results found" />
          Sorry, we couldn't find any results for that search!
        </div>
      )}
      {results.length > 0 && (
        <SearchResults
          index={index}
          results={results}
          CourseHitItem={CourseHitItem}
          ProfessorHitItem={ProfessorHitItem!}
        />
      )}
      <div className="search-pagination">
        <SearchPagination index={index} />
      </div>
    </div>
  );
};

export default SearchHitContainer;
