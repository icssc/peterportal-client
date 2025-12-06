'use client';
import './MobileSearchMenu.scss';
import { FC } from 'react';
import SearchHitContainer from '../SearchHitContainer/SearchHitContainer';
import { useAppSelector } from '../../store/hooks';
import SearchFilters from '../SearchFilters/SearchFilters';
import { ResultsHeader } from '../../app/roadmap/search/SavedAndSearch';

const MobileSearchMenu: FC = () => {
  const inProgressSearch = useAppSelector((state) => state.search.inProgressSearchOperation);
  const hasCompletedQuery = useAppSelector((state) => inProgressSearch !== 'newQuery' && !!state.search.courses.query);
  const showFilters = useAppSelector((state) => hasCompletedQuery && state.search.viewIndex === 'courses');

  return (
    <div className="mobile-search-menu">
      <div className="result-info-container">
        {hasCompletedQuery && <ResultsHeader />}
        {hasCompletedQuery && showFilters && <SearchFilters />}
      </div>
      <SearchHitContainer />
    </div>
  );
};

export default MobileSearchMenu;
