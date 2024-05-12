import { FC, useState } from 'react';
import './AddYearPopup.scss';
import { Plus } from 'react-bootstrap-icons';
import { Button } from 'react-bootstrap';
import YearModal from './YearModal';
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
    <>
      <YearModal
        show={showModal}
        setShow={setShowModal}
        placeholderName={placeholderName}
        placeholderYear={placeholderYear}
        type="add"
        saveHandler={saveHandler}
        currentQuarters={['Fall', 'Winter', 'Spring']}
        // When the year changes, this will force default values to reset
        key={'add-year-' + placeholderYear}
      />
      <Button variant="primary" className="ppc-btn" onClick={() => setShowModal(true)}>
        <Plus className="add-year-icon" />
        <div>Add Year</div>
      </Button>
    </>
  );
};

export default AddYearPopup;
