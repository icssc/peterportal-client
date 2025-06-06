import { FC, useState } from 'react';
import { Button } from 'react-bootstrap';
import './AddYearPopup.scss';

import YearModal from './YearModal';
import { PlannerYearData } from '../../types/types';
import { addYear, selectYearPlans } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';

import AddIcon from '@mui/icons-material/Add';

const AddYearPopup: FC = () => {
  const [showModal, setShowModal] = useState(false);
  const currentPlanData = useAppSelector(selectYearPlans);
  const dispatch = useAppDispatch();

  const placeholderName = 'Year ' + (currentPlanData.length + 1);
  const placeholderYear =
    currentPlanData.length === 0 ? new Date().getFullYear() : currentPlanData[currentPlanData.length - 1].startYear + 1;

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
      <Button variant="light" className="header-btn ppc-btn" onClick={() => setShowModal(true)}>
        <AddIcon />
        <span>Add Year</span>
      </Button>
    </>
  );
};

export default AddYearPopup;
