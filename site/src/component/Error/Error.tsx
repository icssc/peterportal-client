import { FC } from 'react';
import './Error.scss';
import noResultsImg from '../../asset/no-results-crop.webp';
import Image from 'next/image';
import Button from '@mui/material/Button';

interface ErrorProps {
  message?: string;
}

const Error: FC<ErrorProps> = (props) => {
  return (
    <div className="error">
      <Image src={noResultsImg.src} width={noResultsImg.width} height={noResultsImg.height} alt="no results" />
      <h1>404 PAGE NOT FOUND</h1>
      <h2>{props.message}</h2>
      <Button variant="contained" href="/planner">
        Go to Roadmap
      </Button>
    </div>
  );
};

export default Error;
