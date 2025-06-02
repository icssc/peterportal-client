import { FC } from 'react';
import { Spinner } from 'react-bootstrap';

const LoadingPage: FC = () => {
  return (
    <div>
      <div className="loader-container">
        <Spinner animation="border" />
      </div>
    </div>
  );
};

export default LoadingPage;
