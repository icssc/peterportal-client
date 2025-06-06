import { FC, useState, useEffect, useCallback } from 'react';
import './MajorCourseList.scss';
import ProgramRequirementsList from './ProgramRequirementsList';

import trpc from '../../../trpc';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import { setMinorRequirements, MinorRequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch } from '../../../store/hooks';

import ArrowRightIcon from '@mui/icons-material/ArrowRight';
import ArrowDropDownIcon from '@mui/icons-material/ArrowDropDown';

interface MinorCourseListProps {
  minorReqs: MinorRequirements;
}
const MinorCourseList: FC<MinorCourseListProps> = ({ minorReqs }) => {
  const [resultsLoading, setResultsLoading] = useState(false);
  const [open, setOpen] = useState(true);
  const dispatch = useAppDispatch();

  const fetchRequirements = useCallback(
    async (minorId: string) => {
      if (minorReqs.requirements && minorReqs.requirements.length !== 0) return;
      setResultsLoading(true);
      try {
        const requirements = await trpc.programs.getRequiredCourses.query({ type: 'minor', programId: minorId });
        dispatch(setMinorRequirements({ minorId, requirements }));
      } finally {
        setResultsLoading(false);
      }
    },
    [dispatch, minorReqs.requirements],
  );

  useEffect(() => {
    fetchRequirements(minorReqs.minor.id);
  }, [fetchRequirements, minorReqs.minor.id]);

  const toggleExpand = () => setOpen(!open);

  return (
    <div className="major-section">
      <button className="header-tab" onClick={toggleExpand}>
        {open ? <ArrowDropDownIcon /> : <ArrowRightIcon />}
        <h4 className="major-name">{minorReqs.minor.name}</h4>
      </button>
      {open &&
        (resultsLoading ? (
          <LoadingSpinner />
        ) : (
          <ProgramRequirementsList
            requirements={minorReqs.requirements}
            storeKeyPrefix={`minor-${minorReqs.minor.id}`}
          />
        ))}
    </div>
  );
};

export default MinorCourseList;
