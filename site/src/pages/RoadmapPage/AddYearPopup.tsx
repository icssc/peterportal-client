import { FC, useState, useContext } from 'react';
import './AddYearPopup.scss';
import { PlusCircleFill } from 'react-bootstrap-icons';
import { Button } from 'react-bootstrap';
import ThemeContext from '../../style/theme-context';
import CourseYearModal from './CourseYear';
import { addYear } from '../../store/slices/roadmapSlice';
import { useAppDispatch } from '../../store/hooks';
import { PlannerYearData } from '../../types/types';

interface AddYearPopupProps {
  placeholderName: string;
  placeholderYear: number;
}

const AddYearPopup: FC<AddYearPopupProps> = ({ placeholderName, placeholderYear }) => {
  const { darkMode } = useContext(ThemeContext);
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
        // When the year changes, this will force default values to reset
        key={'add-year-' + placeholderYear}
      />
      <Button variant={darkMode ? 'dark' : 'light'} className="add-year-btn" onClick={() => setShowModal(true)}>
        <PlusCircleFill className="add-year-icon" />
        <div className="add-year-text">Add year</div>
      </Button>
    </div>
  );
};

export default AddYearPopup;
