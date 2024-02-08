import { FC, useState } from 'react';
import './AddYearPopup.scss';
import { Plus } from 'react-bootstrap-icons';
import { Button } from 'react-bootstrap';
import CourseYearModal from './CourseYear';
import { addYear } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import { PlannerYearData } from '../../types/types';

interface AddYearPopupProps {
  placeholderName: string;
  placeholderYear: number;
}

const AddYearPopup: FC<AddYearPopupProps> = ({ placeholderName, placeholderYear }) => {
  const [showModal, setShowModal] = useState(false);

  const dispatch = useAppDispatch();
  const saveHandler = (yearData: PlannerYearData) => dispatch(addYear({ yearData }));

  return (
    <div>
      <CourseYearModal
        show={showModal}
        setShow={setShowModal}
        placeholderName={placeholderName}
        placeholderYear={placeholderYear}
        type="add"
        saveHandler={saveHandler}
        currentQuarters={['fall', 'winter', 'spring']}
        // When the year changes, this will force default values to reset
        key={'add-year-' + placeholderYear}
      />
      <Button variant="primary" className="add-year-btn" onClick={() => setShowModal(true)}>
        <Plus className="add-year-icon" />
        <div className="add-year-text">Add Year</div>
      </Button>
    </div>
  );
};

export default AddYearPopup;
