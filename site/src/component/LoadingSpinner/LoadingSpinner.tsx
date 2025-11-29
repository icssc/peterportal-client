import './LoadingSpinner.scss';
import { CircularProgress } from '@mui/material';

const LoadingSpinner = ({ className = '' }: { className?: string }) => {
  return (
    <div className={`loading-spinner ${className}`.trim()}>
      <CircularProgress />
    </div>
  );
};

export default LoadingSpinner;
