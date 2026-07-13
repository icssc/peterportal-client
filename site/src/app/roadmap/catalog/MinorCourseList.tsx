import './MajorCourseList.scss';
import { FC, useCallback, useEffect, useState } from 'react';
import ProgramRequirementsList from './ProgramRequirementsList';
import { DEFAULT_CATALOG_YEAR, formatCatalogYear } from '../../../helpers/courseRequirements';
import {
  setMinorRequirements,
  MinorRequirements,
  setGroupExpanded,
  setMinorCatalogYear,
  setMinorFallbackCatalogYear,
} from '../../../store/slices/courseRequirementsSlice';
import LoadingSpinner from '../../../component/LoadingSpinner/LoadingSpinner';
import WarningAmberIcon from '@mui/icons-material/WarningAmber';
import trpc from '../../../trpc';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';

import { ExpandMore } from '../../../component/ExpandMore/ExpandMore';
import { Collapse, SelectChangeEvent } from '@mui/material';
import ClickableDiv from '../../../component/ClickableDiv/ClickableDiv';
import CatalogYears from './CatalogYears';

function getCoursesForMinor(programId: string, catalogYear?: string) {
  return trpc.programs.getRequiredCourses.query({ type: 'minor', programId, catalogYear });
}

interface MinorCourseListProps {
  minorReqs: MinorRequirements;
  onCatalogYearChange: (minorId: string, catalogYear: string | null) => void;
}

const MinorCourseList: FC<MinorCourseListProps> = ({ minorReqs, onCatalogYearChange }) => {
  const storeKeyPrefix = `minor-${minorReqs.minor.id}`;
  const [resultsLoading, setResultsLoading] = useState(false);
  const fallbackCatalogYear = minorReqs.fallbackCatalogYear ?? null;
  const open = useAppSelector((state) => state.courseRequirements.expandedGroups[storeKeyPrefix] ?? false);
  const setOpen = (isOpen: boolean) => {
    dispatch(setGroupExpanded({ storeKey: storeKeyPrefix, expanded: isOpen }));
  };

  const dispatch = useAppDispatch();

  const fetchRequirements = useCallback(
    async (minorId: string, catalogYear?: string) => {
      const effectiveCatalogYear = catalogYear ?? DEFAULT_CATALOG_YEAR;
      setResultsLoading(true);
      dispatch(setMinorFallbackCatalogYear({ minorId, fallbackCatalogYear: null }));

      try {
        const result = await getCoursesForMinor(minorId, effectiveCatalogYear);
        const { requirements, catalogYear: returnedYear } = result;

        if (returnedYear && returnedYear !== effectiveCatalogYear) {
          dispatch(setMinorFallbackCatalogYear({ minorId, fallbackCatalogYear: returnedYear }));
        }

        dispatch(setMinorRequirements({ minorId, requirements }));
      } finally {
        setResultsLoading(false);
      }
    },
    [dispatch],
  );

  useEffect(() => {
    if (!minorReqs.requirements || minorReqs.requirements.length === 0) {
      fetchRequirements(minorReqs.minor.id, minorReqs.catalogYear ?? undefined);
    }
  }, [fetchRequirements, minorReqs.minor.id, minorReqs.requirements, minorReqs.catalogYear]);

  const toggleExpand = () => setOpen(!open);

  const handleCatalogYearChange = useCallback(
    async (event: SelectChangeEvent) => {
      const newCatalogYear = event.target.value || null;
      if (newCatalogYear === minorReqs.catalogYear) return;

      setResultsLoading(true);
      onCatalogYearChange(minorReqs.minor.id, newCatalogYear);
      dispatch(setMinorRequirements({ minorId: minorReqs.minor.id, requirements: [] }));
      dispatch(setMinorCatalogYear({ minorId: minorReqs.minor.id, catalogYear: newCatalogYear }));
      await fetchRequirements(minorReqs.minor.id, newCatalogYear ?? undefined);
    },
    [dispatch, fetchRequirements, minorReqs.minor.id, minorReqs.catalogYear, onCatalogYearChange],
  );

  return (
    <div className="major-section">
      <ClickableDiv className="header-tab" onClick={toggleExpand}>
        <h4 className="major-name">{minorReqs.minor.name}</h4>
        <ExpandMore className="expand-requirements" expanded={open} onClick={toggleExpand} />
      </ClickableDiv>
      <Collapse in={open} unmountOnExit>
        <CatalogYears catalogYear={minorReqs.catalogYear} tab="Minor" onChange={handleCatalogYearChange} />
        {fallbackCatalogYear && !resultsLoading && (
          <div className="catalog-year-warning">
            <WarningAmberIcon className="warning-icon" />
            <p className="catalog-year-warning-text">
              {formatCatalogYear(DEFAULT_CATALOG_YEAR)} requirements are not yet publicly available. Currently showing{' '}
              {formatCatalogYear(fallbackCatalogYear)}.
            </p>
          </div>
        )}
        {resultsLoading ? (
          <LoadingSpinner />
        ) : (
          <ProgramRequirementsList requirements={minorReqs.requirements} storeKeyPrefix={storeKeyPrefix} />
        )}
      </Collapse>
    </div>
  );
};

export default MinorCourseList;
