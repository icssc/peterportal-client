import './MajorCourseList.scss';
import { FC, useCallback, useEffect, useState, useMemo } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import { normalizeMajorName } from '../../../helpers/courseRequirements';
import {
  MajorWithSpecialization,
  setGroupExpanded,
  setMajorCatalogYear,
  setMajorSpecs,
  setRequirements,
  setSpecialization,
} from '../../../store/slices/courseRequirementsSlice';
import { MajorSpecialization } from '@peterportal/types';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import trpc from '../../../trpc';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';

import { ExpandMore } from '../../../component/ExpandMore/ExpandMore';
import { Autocomplete, Collapse, FormControl, MenuItem, Select, SelectChangeEvent, TextField } from '@mui/material';
import ClickableDiv from '../../../component/ClickableDiv/ClickableDiv';

const noSpecId = 'NO_SPEC';

const DEFAULT_CATALOG_YEAR = '20252026';

const loadingSpecValue = {
  value: {
    id: 'loading_spec',
    majorId: '',
    name: '',
  },
  label: 'Loading...',
};

function getMajorSpecializations(majorId: string) {
  return trpc.programs.getSpecializations.query({ major: majorId });
}

function getCoursesForMajor(programId: string, specId: string | undefined, catalogYear?: string) {
  const specializationId = specId === noSpecId ? undefined : specId;
  return trpc.programs.getRequiredCourses.query({
    type: 'major',
    programId,
    specializationId,
    catalogYear: catalogYear ?? DEFAULT_CATALOG_YEAR,
  });
}

function getCoursesForSpecialization(programId?: string | null) {
  if (!programId || programId === noSpecId) return [];
  return trpc.programs.getRequiredCourses.query({ type: 'specialization', programId });
}

interface MajorCourseListProps {
  majorWithSpec: MajorWithSpecialization;
  onSpecializationChange: (majorId: string, spec: MajorSpecialization | null) => void;
  selectedSpecId?: string;
  onCatalogYearChange: (majorId: string, catalogYear: string | null) => void;
}

const MajorCourseList: FC<MajorCourseListProps> = ({
  majorWithSpec,
  onSpecializationChange,
  selectedSpecId,
  onCatalogYearChange,
}) => {
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
  const noSpec = useMemo(() => ({ id: noSpecId, majorId: major.id, name: 'No Specialization' }), [major.id]);

  if (specOptions.length > 0 && !major.specializationRequired) {
    specOptions.unshift({ value: noSpec, label: noSpec.name });
  }

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
    async (majorId: string, specId?: string, catalogYear?: string) => {
      setResultsLoading(true);

      try {
        const requirements = await getCoursesForMajor(majorId, specId, catalogYear);
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
      else return await fetchRequirements(major.id, undefined, majorWithSpec.catalogYear ?? undefined);
    }
    if (!selectedSpecId && !selectedSpec?.id) return;
    if (selectedSpecId === selectedSpec?.id) return;

    const specs = await getMajorSpecializations(major.id);
    const foundSpec = specs.find((s) => s.id === selectedSpecId);

    if (foundSpec) {
      dispatch(setSpecialization({ majorId: major.id, specialization: foundSpec }));
      await fetchRequirements(major.id, foundSpec?.id, majorWithSpec.catalogYear ?? undefined);
    } else if (selectedSpecId === noSpecId) {
      dispatch(setSpecialization({ majorId: major.id, specialization: noSpec }));
      await fetchRequirements(major.id, undefined, majorWithSpec.catalogYear ?? undefined);
    }
  }, [
    dispatch,
    fetchRequirements,
    hasSpecs,
    noSpec,
    major.id,
    majorWithSpec.requirements.length,
    majorWithSpec.catalogYear,
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
      await fetchRequirements(major.id, updatedSpec?.id, majorWithSpec.catalogYear ?? undefined);
    },
    [dispatch, fetchRequirements, major, majorWithSpec.catalogYear, onSpecializationChange, selectedSpecId],
  );

  const toggleExpand = () => setOpen(!open);
  const selectedSpecOption = specOptions.find(
    (s) => s.value.id === (majorWithSpec.selectedSpec?.id ?? selectedSpec?.id),
  );

  const handleCatalogYearChange = useCallback(
    async (event: SelectChangeEvent) => {
      const newCatalogYear = event.target.value || null;
      if (newCatalogYear === majorWithSpec.catalogYear) return;

      setResultsLoading(true);
      onCatalogYearChange(major.id, newCatalogYear);
      dispatch(setRequirements({ majorId: major.id, requirements: [] }));
      dispatch(setMajorCatalogYear({ majorId: major.id, catalogYear: newCatalogYear }));
      await fetchRequirements(major.id, selectedSpec?.id, newCatalogYear ?? undefined);
    },
    [dispatch, fetchRequirements, major.id, majorWithSpec.catalogYear, onCatalogYearChange, selectedSpec?.id],
  );
  return (
    <div className="major-section">
      <ClickableDiv className="header-tab" onClick={toggleExpand}>
        <h4 className="major-name">{major.name}</h4>
        <ExpandMore className="expand-requirements" expanded={open} onClick={toggleExpand} />
      </ClickableDiv>
      <Collapse in={open} unmountOnExit>
        <h5 className="catalog-year-title">Catalog Year</h5>
        <FormControl className="catalog-year-dropdown" fullWidth>
          <Select
            labelId="catalog-year-select-label"
            id="catalog-year-select"
            value={majorWithSpec.catalogYear ?? DEFAULT_CATALOG_YEAR}
            onChange={handleCatalogYearChange}
          >
            <MenuItem value="20222023">2022-2023</MenuItem>
            <MenuItem value="20262027">2026-2027</MenuItem>
          </Select>
        </FormControl>
        {hasSpecs && (
          <Autocomplete
            className="specialization-select"
            disableClearable
            options={specOptions}
            value={selectedSpecOption ?? loadingSpecValue}
            inputValue={selectedSpecOption?.label ?? ''}
            filterOptions={(options) => options}
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
