import { FC } from 'react';
import SubReview from '../../../component/Review/ReviewCard';
import { Button } from '@mui/material';
import DeleteForeverIcon from '@mui/icons-material/DeleteForever';
import CheckIcon from '@mui/icons-material/Check';
import './UnverifiedReview.scss';
import { ReviewData } from '@peterportal/types';

const UnverifiedReview: FC<{ review: ReviewData; onDelete: () => void; onVerify: () => void }> = ({
  review,
  onDelete,
  onVerify,
}) => {
  return (
    <SubReview review={review}>
      <div className="verification-buttons">
        <Button className="ppc-mui-button" variant="text" onClick={onDelete} startIcon={<DeleteForeverIcon />}>
          Delete
        </Button>
        <Button className="ppc-mui-button primary-button" variant="text" onClick={onVerify} startIcon={<CheckIcon />}>
          Verify
        </Button>
      </div>
    </SubReview>
  );
};

export default UnverifiedReview;
