import { FC, useState, useEffect, useCallback } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';

import trpc from '../../../trpc';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import { setGERequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { normalizeTitleLabels } from '../../../helpers/substitutions';

const GERequiredCourseList: FC = () => {
  const requirements = useAppSelector((state) => state.courseRequirements.geRequirements);
  const [resultsLoading, setResultsLoading] = useState(false);
  const dispatch = useAppDispatch();

  const fetchRequirements = useCallback(async () => {
    if (requirements.length) return;
    setResultsLoading(true);
    try {
      const geReqs = await trpc.programs.getRequiredCoursesUgrad.query({ id: 'GE' });
      normalizeTitleLabels(geReqs);
      dispatch(setGERequirements(geReqs));
    } finally {
      setResultsLoading(false);
    }
  }, [dispatch, requirements]);

  useEffect(() => {
    fetchRequirements();
  }, [fetchRequirements]);

  if (resultsLoading) {
    return <LoadingSpinner />;
  }

  return <ProgramRequirementsList requirements={requirements} storeKeyPrefix="ge" />;
};

export default GERequiredCourseList;
