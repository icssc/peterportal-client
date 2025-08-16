import { Spinner } from 'react-bootstrap';
import './LoadingSpinner.scss';

const LoadingSpinner = ({ className = '' }: { className?: string }) => {
  return (
    <div className={`loading-spinner ${className}`.trim()}>
      <Spinner animation="border" role="status" />
    </div>
  );
};

export default LoadingSpinner;
