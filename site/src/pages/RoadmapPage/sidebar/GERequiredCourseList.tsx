import { FC, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import trpc from '../../../trpc';
import RequirementsLoadingIcon from './RequirementsLoadingIcon';
import { setGERequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { findCommonLabelPrefix } from '../../../helpers/substitutions';

async function getCoursesForGE() {
  const fetchedCourses = await trpc.programs.getRequiredCoursesUgrad.query({ id: 'GE' });

  /* if a top level "Select 1 of the following" group of requirements is seen, attempt to replace the label */
  fetchedCourses.forEach((geRequirement) => {
    if (geRequirement.requirementType === 'Group' && geRequirement.label === 'Select 1 of the following') {
      const requirementSubLabels = geRequirement.requirements.map((req) => req.label);

      const commonLabelPrefix = findCommonLabelPrefix(requirementSubLabels);

      if (commonLabelPrefix) {
        geRequirement.label = commonLabelPrefix; // if a common label prefix is found, use it as the top level label
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
