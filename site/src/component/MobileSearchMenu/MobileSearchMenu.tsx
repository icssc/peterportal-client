'use client';
import { FC } from 'react';
import SearchHitContainer from '../SearchHitContainer/SearchHitContainer';
import { Dialog } from '@mui/material';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { setCourse } from '../../store/slices/popupSlice';
import CoursePreview from '../CoursePreview/CoursePreview';

const MobileSearchMenu: FC = () => {
  const dispatch = useAppDispatch();
  const selectedCourse = useAppSelector((state) => state.popup.course);

  const handleClose = () => {
    dispatch(setCourse(null));
  };

  return (
    <div className="mobile-search-menu">
      <SearchHitContainer index="courses" />
      <Dialog fullScreen={true} open={selectedCourse != null} onClose={handleClose} className="mobile-search-dialog">
        <CoursePreview courseId={selectedCourse ? selectedCourse.id : ''} onClose={handleClose} />
      </Dialog>
    </div>
  );
};

export default MobileSearchMenu;
