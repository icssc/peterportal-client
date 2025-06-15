import { FC } from 'react';
import './Error.scss';
import noResultsImg from '../../asset/no-results-crop.webp';

interface ErrorProps {
  message?: string;
}

const Error: FC<ErrorProps> = ({ message = '' }) => {
  return (
    <div className="error">
      <img src={noResultsImg} alt="No results found" />
      <h1>404 PAGE NOT FOUND</h1>
      <h2>{message}</h2>
    </div>
  );
};

export default Error;
