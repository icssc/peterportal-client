import './MajorCourseList.scss';
import { FC, useCallback, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import { setMinorRequirements, MinorRequirements } from '../../../store/slices/courseRequirementsSlice';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import trpc from '../../../trpc';
import { useAppDispatch } from '../../../store/hooks';

import { ExpandMore } from '../../../component/ExpandMore/ExpandMore';
import { Collapse } from '@mui/material';

function getCoursesForMinor(programId: string) {
  return trpc.programs.getRequiredCourses.query({ type: 'minor', programId });
}

interface MinorCourseListProps {
  minorReqs: MinorRequirements;
}

const MinorCourseList: FC<MinorCourseListProps> = ({ minorReqs }) => {
  const [resultsLoading, setResultsLoading] = useState(false);
  const [open, setOpen] = useState(true);

  const dispatch = useAppDispatch();

  const fetchRequirements = useCallback(
    async (minorId: string) => {
      setResultsLoading(true);

      try {
        const requirements = await getCoursesForMinor(minorId);
        dispatch(setMinorRequirements({ minorId, requirements }));
      } finally {
        setResultsLoading(false);
      }
    },
    [dispatch],
  );

  useEffect(() => {
    if (!minorReqs.requirements || minorReqs.requirements.length === 0) {
      fetchRequirements(minorReqs.minor.id);
    }
  }, [fetchRequirements, minorReqs.minor.id, minorReqs.requirements]);

  const toggleExpand = () => setOpen(!open);

  const renderRequirements = () => {
    if (resultsLoading) return <LoadingSpinner />;
    return (
      <ProgramRequirementsList requirements={minorReqs.requirements} storeKeyPrefix={`minor-${minorReqs.minor.id}`} />
    );
  };

  return (
    <div className="major-section">
      <button className="header-tab" onClick={toggleExpand}>
        <h4 className="major-name">{minorReqs.minor.name}</h4>
        <ExpandMore className="expand-requirements" expanded={open} onClick={toggleExpand} />
      </button>
      <Collapse in={open} unmountOnExit>
        {renderRequirements()}
      </Collapse>
    </div>
  );
};

export default MinorCourseList;
