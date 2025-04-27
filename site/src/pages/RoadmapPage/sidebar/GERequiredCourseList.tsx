import { FC, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import trpc from '../../../trpc';
import RequirementsLoadingIcon from './RequirementsLoadingIcon';
import { setGERequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { getCommonLabelPrefix, expandAbbreviation } from '../../../helpers/substitutions';

async function getCoursesForGE() {
  const fetchedCourses = await trpc.programs.getRequiredCoursesUgrad.query({ id: 'GE' });

  fetchedCourses.forEach((courseBlock) => {
    if (courseBlock.label === 'Select 1 of the following' && courseBlock.requirementType === 'Group') {
      const requirementLabels = courseBlock.requirements
        .filter((req) => req.requirementType === 'Group')
        .map((req) => req.label);

      const commonBase = getCommonLabelPrefix(requirementLabels);

      if (commonBase) {
        courseBlock.label = expandAbbreviation(commonBase.trim());
      }
    }
  });

  return fetchedCourses;
}

const GERequiredCourseList: FC = () => {
  const requirements = useAppSelector((state) => state.courseRequirements.geRequirements);

  const [resultsLoading, setResultsLoading] = useState(false);

  const dispatch = useAppDispatch();

  // Initially fetch requirements
  useEffect(() => {
    if (requirements.length) return;

    setResultsLoading(true);
    getCoursesForGE().then((geReqs) => {
      dispatch(setGERequirements(geReqs));
      setResultsLoading(false);
    });
  }, [dispatch, requirements]);

  return (
    <>
      {resultsLoading ? (
        <RequirementsLoadingIcon />
      ) : (
        <ProgramRequirementsList requirements={requirements} storeKeyPrefix="ge" />
      )}
    </>
  );
};

export default GERequiredCourseList;
