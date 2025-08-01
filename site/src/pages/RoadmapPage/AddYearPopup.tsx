'use client';
import { FC, useState } from 'react';
import './AddYearPopup.scss';
import YearModal from './YearModal';
import { addYear, selectYearPlans } from '../../store/slices/roadmapSlice';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { PlannerYearData } from '../../types/types';
import AddIcon from '@mui/icons-material/Add';
import { Button } from '@mui/material';

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
      <Button variant="text" className="header-btn" onClick={() => setShowModal(true)} startIcon={<AddIcon />}>
        <span>Add Year</span>
      </Button>
    </>
  );
};

export default AddYearPopup;
