'use client';
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
  const selectedCourse = useAppSelector((state) => state.popup.course);
  const showFilters = useAppSelector((state) => state.search.viewIndex === 'courses' && state.search.courses.query);

  // TODO 12-2 fix styles + don't auto switch to profs in course catalog view (or rather, don't trigger search at all there)

  const handleClose = () => {
    dispatch(setCourse(null));
  };

  return (
    <div className="mobile-search-menu">
      <ResultsHeader />
      {showFilters && <SearchFilters />}
      <SearchHitContainer />
      <MobilePopup show={!!selectedCourse} onClose={handleClose}>
        {selectedCourse && <CoursePreview courseId={selectedCourse.id} onClose={handleClose} />}
      </MobilePopup>
    </div>
  );
};

export default MobileSearchMenu;
