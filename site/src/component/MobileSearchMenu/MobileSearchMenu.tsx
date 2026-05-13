'use client';
import './MobileSearchMenu.scss';
import { FC } from 'react';
import { useAppSelector } from '../../store/hooks';
import { ResultsHeader } from '../../app/roadmap/search/SavedAndSearch';
import SearchHitContainer from '../SearchHitContainer/SearchHitContainer';
import SearchFilters from '../SearchFilters/SearchFilters';
import ScrollToTopButton from '../ScrollToTopButton/ScrollToTopButton';

const MobileSearchMenu: FC = () => {
  const inProgressSearch = useAppSelector((state) => state.search.inProgressSearchOperation);
  const hasCompletedQuery = useAppSelector((state) => inProgressSearch !== 'newQuery' && !!state.search.courses.query);
  const hasQuery = useAppSelector((state) => !!state.search.courses.query);
  const filtersDisabled = useAppSelector((state) => hasQuery && state.search.viewIndex === 'instructors');
  const filtersDisabledReason = filtersDisabled ? 'Filters apply to course results only.' : undefined;

  return (
    <div className="mobile-search-menu">
      <div className="result-info-container">
        <SearchFilters
          disabled={filtersDisabled}
          disabledReason={filtersDisabledReason}
          addTopPadding={!hasCompletedQuery}
        />
        {hasCompletedQuery && <ResultsHeader />}
      </div>
      <SearchHitContainer />
      <ScrollToTopButton scrollableTarget="mobileScrollContainer" />
    </div>
  );
};

export default MobileSearchMenu;
