import { Spinner } from 'react-bootstrap';
import './LoadingSpinner.scss';

const LoadingSpinner = () => {
  return (
    <div className="loading-spinner">
      <Spinner animation="border" role="status" />
    </div>
  );
};

export default LoadingSpinner;
