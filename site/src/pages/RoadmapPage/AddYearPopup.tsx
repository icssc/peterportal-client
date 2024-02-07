import { FC, useState, useContext } from 'react';
import './AddYearPopup.scss';
import { PlusCircleFill } from 'react-bootstrap-icons';
import { Button } from 'react-bootstrap';
import ThemeContext from '../../style/theme-context';
import CourseYearModal from './CourseYear';

interface AddYearPopupProps {
  placeholderName: string;
  placeholderYear: number;
}

const AddYearPopup: FC<AddYearPopupProps> = ({ placeholderName, placeholderYear }) => {
  const { darkMode } = useContext(ThemeContext);
  const [showModal, setShowModal] = useState(false);

  return (
    <div>
      <CourseYearModal
        show={showModal}
        setShow={setShowModal}
        placeholderName={placeholderName}
        placeholderYear={placeholderYear}
      />
      <Button variant={darkMode ? 'dark' : 'light'} className="add-year-btn" onClick={() => setShowModal(true)}>
        <PlusCircleFill className="add-year-icon" />
        <div className="add-year-text">Add year</div>
      </Button>
    </div>
  );
};

export default AddYearPopup;
