'use client';
import { FC } from 'react';
import SearchHitContainer from '../SearchHitContainer/SearchHitContainer';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import CoursePreview from '../ResultPreview/CoursePreview';
import MobilePopup from '../../app/roadmap/MobilePopup';

const MobileSearchMenu: FC = () => {
  const dispatch = useAppDispatch();
  const selectedCourse = useAppSelector((state) => state.popup.course);

  const handleClose = () => {
    dispatch(setCourse(null));
  };

  return (
    <div className="mobile-search-menu">
      <SearchHitContainer index="courses" />
      <MobilePopup show={!!selectedCourse} onClose={handleClose}>
        {selectedCourse && <CoursePreview courseId={selectedCourse.id} onClose={handleClose} />}
      </MobilePopup>
    </div>
  );
};

export default MobileSearchMenu;
