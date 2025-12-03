'use client';
import './MobileSearchMenu.scss';
import { FC } from 'react';
import SearchHitContainer from '../SearchHitContainer/SearchHitContainer';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import CoursePreview from '../ResultPreview/CoursePreview';
import MobilePopup from '../../app/roadmap/MobilePopup';
import SearchFilters from '../SearchFilters/SearchFilters';
import { ResultsHeader } from '../../app/roadmap/search/SavedAndSearch';

const MobileSearchMenu: FC = () => {
  const dispatch = useAppDispatch();
  const inProgressSearch = useAppSelector((state) => state.search.inProgressSearchOperation);
  const selectedCourse = useAppSelector((state) => state.popup.course);
  const hasCompletedQuery = useAppSelector((state) => inProgressSearch === 'none' && !!state.search.courses.query);
  const showFilters = useAppSelector((state) => hasCompletedQuery && state.search.viewIndex === 'courses');

  const handleClose = () => {
    dispatch(setCourse(null));
  };

  return (
    <div className="mobile-search-menu">
      <div className="result-info-container">
        {hasCompletedQuery && <ResultsHeader />}
        {hasCompletedQuery && showFilters && <SearchFilters />}
      </div>
      <SearchHitContainer />
      <MobilePopup show={!!selectedCourse} onClose={handleClose}>
        {selectedCourse && <CoursePreview courseId={selectedCourse.id} onClose={handleClose} />}
      </MobilePopup>
    </div>
  );
};

export default MobileSearchMenu;
