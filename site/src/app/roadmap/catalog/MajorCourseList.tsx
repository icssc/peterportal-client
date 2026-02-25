import './MajorCourseList.scss';
import { FC, useCallback, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import { normalizeMajorName } from '../../../helpers/courseRequirements';
import {
  MajorWithSpecialization,
  setGroupExpanded,
  setMajorSpecs,
  setRequirements,
  setSpecialization,
} from '../../../store/slices/courseRequirementsSlice';
import { MajorSpecialization } from '@peterportal/types';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import trpc from '../../../trpc';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';

import { ExpandMore } from '../../../component/ExpandMore/ExpandMore';
import { Autocomplete, Collapse, TextField } from '@mui/material';

function getMajorSpecializations(majorId: string) {
  return trpc.programs.getSpecializations.query({ major: majorId });
}

function getCoursesForMajor(programId: string) {
  return trpc.programs.getRequiredCourses.query({ type: 'major', programId });
}

function getCoursesForSpecialization(programId?: string | null) {
  if (!programId) return [];
  return trpc.programs.getRequiredCourses.query({ type: 'specialization', programId });
}

interface MajorCourseListProps {
  majorWithSpec: MajorWithSpecialization;
  onSpecializationChange: (majorId: string, spec: MajorSpecialization | null) => void;
  selectedSpecId?: string;
}

const MajorCourseList: FC<MajorCourseListProps> = ({ majorWithSpec, onSpecializationChange, selectedSpecId }) => {
  const storeKeyPrefix = `major-${majorWithSpec.major.id}`;
  const [specsLoading, setSpecsLoading] = useState(false);
  const [resultsLoading, setResultsLoading] = useState(false);
  const open = useAppSelector((state) => state.courseRequirements.expandedGroups[storeKeyPrefix] ?? false);
  const setOpen = (isOpen: boolean) => {
    dispatch(setGroupExpanded({ storeKey: storeKeyPrefix, expanded: isOpen }));
  };

  const { major, selectedSpec, specializations } = majorWithSpec;
  const hasSpecs = major.specializations.length > 0;
  const specOptions = specializations.map((s) => ({ value: s, label: s.name }));

  const dispatch = useAppDispatch();

  const loadSpecs = useCallback(async () => {
    setSpecsLoading(true);
    try {
      const specs = await getMajorSpecializations(major.id);
      specs.forEach((s) => (s.name = normalizeMajorName(s)));
      specs.sort((a, b) => a.name.localeCompare(b.name));
      dispatch(setMajorSpecs({ majorId: major.id, specializations: specs }));
    } finally {
      setSpecsLoading(false);
    }
  }, [dispatch, major.id]);

  const fetchRequirements = useCallback(
    async (majorId: string, specId?: string | null) => {
      setResultsLoading(true);

      try {
        const requirements = await getCoursesForMajor(majorId);
        requirements.push(...(await getCoursesForSpecialization(specId)));
        dispatch(setRequirements({ majorId, requirements }));
      } finally {
        setResultsLoading(false);
      }
    },
    [dispatch],
  );

  const loadSpecRequirements = useCallback(async () => {
    if (!hasSpecs) {
      if (majorWithSpec.requirements.length > 0) return;
      else return await fetchRequirements(major.id, null);
    }
    if (!selectedSpecId && !selectedSpec?.id) return;
    if (selectedSpecId === selectedSpec?.id) return;

    const specs = await getMajorSpecializations(major.id);
    const foundSpec = specs.find((s) => s.id === selectedSpecId);
    if (foundSpec) {
      dispatch(setSpecialization({ majorId: major.id, specialization: foundSpec }));
      await fetchRequirements(major.id, foundSpec?.id);
    }
  }, [
    dispatch,
    fetchRequirements,
    hasSpecs,
    major.id,
    majorWithSpec.requirements.length,
    selectedSpecId,
    selectedSpec?.id,
  ]);

  // Initial Loader
  useEffect(() => {
    if (specOptions.length) return;
    if (hasSpecs && !specOptions.length) {
      loadSpecs().then(loadSpecRequirements);
    } else {
      loadSpecRequirements();
    }
  }, [hasSpecs, loadSpecRequirements, specOptions.length, loadSpecs]);

  const handleSpecializationChange = useCallback(
    async (data: { value: MajorSpecialization; label: string } | null) => {
      const updatedSpec = data?.value ?? null;
      if (updatedSpec?.id === selectedSpecId) return;

      setResultsLoading(true);
      onSpecializationChange(major.id, updatedSpec);
      dispatch(setRequirements({ majorId: major.id, requirements: [] }));
      dispatch(setSpecialization({ majorId: major.id, specialization: updatedSpec }));
      await fetchRequirements(major.id, updatedSpec?.id ?? null);
    },
    [dispatch, fetchRequirements, major, onSpecializationChange, selectedSpecId],
  );

  const selectedSpecIdOrSelected = majorWithSpec.selectedSpec?.id ?? selectedSpecId ?? '';

  const toggleExpand = () => setOpen(!open);

  return (
    <div className="major-section">
      <div
        className="header-tab"
        role="button"
        tabIndex={0}
        onClick={toggleExpand}
        onKeyDown={(e) => {
          if (e.key === 'Enter' || e.key === ' ') toggleExpand();
        }}
      >
        <h4 className="major-name">{major.name}</h4>
        <ExpandMore className="expand-requirements" expanded={open} onClick={toggleExpand} />
      </div>
      <Collapse in={open} unmountOnExit>
        {hasSpecs && (
          <Autocomplete
            className="specialization-select"
            options={specOptions}
            value={specOptions.find((s) => s.value.id === selectedSpecIdOrSelected) ?? null}
            onChange={(_event, option) => handleSpecializationChange(option)}
            getOptionLabel={(option) => option.label}
            isOptionEqualToValue={(option, value) => option.value.id === value.value.id}
            disabled={specsLoading}
            loading={specsLoading}
            renderInput={(params) => (
              <TextField {...params} variant="outlined" size="small" placeholder="Select a specialization..." />
            )}
          />
        )}
        {hasSpecs && !majorWithSpec.selectedSpec ? (
          <p className="unselected-spec-notice">Please select a specialization to view requirements</p>
        ) : resultsLoading ? (
          <LoadingSpinner />
        ) : (
          <ProgramRequirementsList requirements={majorWithSpec.requirements} storeKeyPrefix={storeKeyPrefix} />
        )}
      </Collapse>
    </div>
  );
};

export default MajorCourseList;
