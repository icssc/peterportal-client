'use client';
import './MobileSearchMenu.scss';
import { FC } from 'react';
import SearchHitContainer from '../SearchHitContainer/SearchHitContainer';
import { useAppSelector } from '../../store/hooks';
import SearchFilters from '../SearchFilters/SearchFilters';
import { ResultsHeader } from '../../app/roadmap/search/SavedAndSearch';

interface MobileSearchMenuProps {
  mobileScrollContainerRef: React.RefObject<HTMLDivElement>;
}

const MobileSearchMenu: FC<MobileSearchMenuProps> = ({ mobileScrollContainerRef }) => {
  const inProgressSearch = useAppSelector((state) => state.search.inProgressSearchOperation);
  const hasCompletedQuery = useAppSelector((state) => inProgressSearch !== 'newQuery' && !!state.search.courses.query);
  const showFilters = useAppSelector((state) => hasCompletedQuery && state.search.viewIndex === 'courses');

  return (
    <div className="mobile-search-menu">
      <div className="result-info-container">
        {hasCompletedQuery && <ResultsHeader />}
        {hasCompletedQuery && showFilters && <SearchFilters />}
      </div>
      <SearchHitContainer mobileScrollContainerRef={mobileScrollContainerRef} />
    </div>
  );
};

export default MobileSearchMenu;
