import './RequirementsLoadingIcon.scss';
import { Spinner } from 'react-bootstrap';

const RequirementsLoadingIcon = () => {
  return (
    <div className="requirements-loading">
      <Spinner animation="border" />
    </div>
  );
};

export default RequirementsLoadingIcon;
