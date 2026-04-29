import { useEffect, useState, useCallback } from 'react';
import type { FormEvent } from 'react';
import { useSearchParams } from 'next/navigation';
import {
  Box,
  Button,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  FormControl,
  InputLabel,
  MenuItem,
  Select,
  Typography,
  Divider,
} from '@mui/material';
import type { SelectChangeEvent } from '@mui/material/Select';
import { IosShare } from '@mui/icons-material';
import { CircularProgress } from '@mui/material';
import { quarterDisplayNames } from '../../helpers/planner';
import { type QuarterName } from '@peterportal/types';
import { useAppSelector, useAppDispatch } from '../../store/hooks';
import {
  selectAllPlans,
  selectCurrentPlan,
  setToastMsg,
  setShowToast,
  setToastSeverity,
} from '../../store/slices/roadmapSlice';
import { useSaveRoadmap } from '../../hooks/planner';
import { useIsLoggedIn } from '../../hooks/isLoggedIn';

const getDefaultExportSelection = (
  roadmapYears: { startYear: number; quarters: { name: QuarterName; courses?: unknown[] }[] }[],
) => {
  const firstYear = roadmapYears.find((y) => y.quarters.some((q) => (q.courses ?? []).length > 0));
  if (!firstYear) return { yearStart: '', quarterName: '' };

  const firstQuarter = firstYear.quarters.find((q) => (q.courses ?? []).length > 0);
  return {
    yearStart: String(firstYear.startYear),
    quarterName: firstQuarter?.name ?? '',
  };
};

const quarterYearOffsets: Record<QuarterName, number> = {
  Fall: 0,
  Winter: 1,
  Spring: 1,
  Summer1: 1,
  Summer2: 1,
  Summer10wk: 1,
};

const YearDisplayWithRange = ({ year }: { year: { name: string; startYear: number } }) => (
  <Box sx={{ display: 'flex', flexDirection: 'row', gap: 1, alignItems: 'center' }}>
    <Typography variant="body2">{year.name}</Typography>
    <Typography variant="caption" sx={{ color: 'text.secondary' }}>
      {year.startYear}-{year.startYear + 1}
    </Typography>
  </Box>
);

const ExportButton = () => {
  const searchParams = useSearchParams();
  const [showModal, setShowModal] = useState(false);
  const [selectedRoadmapId, setSelectedRoadmapId] = useState('');
  const [selectedYearStart, setSelectedYearStart] = useState('');
  const [selectedQuarterName, setSelectedQuarterName] = useState('');
  const [autoExport, setAutoExport] = useState(false);
  const allPlans = useAppSelector(selectAllPlans);
  const currentPlan = useAppSelector(selectCurrentPlan);
  const { handler: saveRoadmap } = useSaveRoadmap();
  const isLoggedIn = useIsLoggedIn();
  const dispatch = useAppDispatch();
  const [saving, setSaving] = useState(false);

  const selectedRoadmap = allPlans.find((plan) => String(plan.id) === selectedRoadmapId);
  const roadmapYears = selectedRoadmap?.content.yearPlans ?? [];
  const selectedYear = roadmapYears.find((year) => String(year.startYear) === selectedYearStart);
  const yearQuarters = selectedYear?.quarters ?? [];

  const handleClose = () => {
    setShowModal(false);
    setSelectedRoadmapId('');
    setSelectedYearStart('');
    setSelectedQuarterName('');
  };

  const handleRoadmapChange = (event: SelectChangeEvent<string>) => {
    const roadmapId = event.target.value;
    setSelectedRoadmapId(roadmapId);

    const roadmap = allPlans.find((plan) => String(plan.id) === roadmapId);
    const nextSelection = roadmap
      ? getDefaultExportSelection(roadmap.content.yearPlans)
      : { yearStart: '', quarterName: '' };
    setSelectedYearStart(nextSelection.yearStart);
    setSelectedQuarterName(nextSelection.quarterName);
  };

  const handleYearChange = (event: SelectChangeEvent<string>) => {
    const yearStart = event.target.value;
    setSelectedYearStart(yearStart);

    const year = roadmapYears.find((planYear) => String(planYear.startYear) === yearStart);
    const firstQuarterWithCourses = year?.quarters.find((q) => (q.courses ?? []).length > 0);
    setSelectedQuarterName(firstQuarterWithCourses?.name ?? '');
  };

  const handleQuarterChange = (event: SelectChangeEvent<string>) => {
    setSelectedQuarterName(event.target.value);
  };

  const handleExport = useCallback(async () => {
    if (!selectedRoadmap || !selectedYear || !selectedQuarterName) return;
    const quarterName = selectedQuarterName as QuarterName;

    // If user is not logged in, redirect to auth with export params preserved
    if (!isLoggedIn) {
      dispatch(setToastMsg('Sign in to export your roadmap'));
      dispatch(setToastSeverity('info'));
      dispatch(setShowToast(true));
      const returnUrl = new URL(window.location.href);
      returnUrl.searchParams.set('exportRoadmapId', selectedRoadmapId);
      returnUrl.searchParams.set('exportYearStart', selectedYearStart);
      returnUrl.searchParams.set('exportQuarterName', selectedQuarterName);
      const returnTo = encodeURIComponent(returnUrl.toString());
      window.location.assign(`/planner/api/users/auth/google?next=${returnTo}`);
      return;
    }

    let plannerIdToUse: number | undefined = selectedRoadmap.id;

    setSaving(true);
    try {
      if ((selectedRoadmap.id ?? -1) <= 0) {
        const result = await saveRoadmap();
        if (!result || !result.success) {
          dispatch(setToastMsg('Unable to save roadmap before export'));
          dispatch(setToastSeverity('error'));
          dispatch(setShowToast(true));
          return;
        }

        const tempId = selectedRoadmap.id ?? -1;
        plannerIdToUse = result.plannerIdLookup?.[tempId] ?? plannerIdToUse;

        if (!plannerIdToUse || plannerIdToUse <= 0) {
          dispatch(setToastMsg('Please save the roadmap to your account before exporting'));
          dispatch(setToastSeverity('info'));
          dispatch(setShowToast(true));
          return;
        }
      } else {
        await saveRoadmap();
      }
    } finally {
      setSaving(false);
    }

    const url = new URL('/', window.location.origin);
    url.searchParams.set('importRoadmap', String(plannerIdToUse));
    url.searchParams.set('term', `${parseInt(selectedYearStart, 10) + quarterYearOffsets[quarterName]} ${quarterName}`);

    window.location.assign(url.toString());
  }, [
    selectedRoadmap,
    selectedYear,
    selectedQuarterName,
    isLoggedIn,
    selectedRoadmapId,
    selectedYearStart,
    dispatch,
    saveRoadmap,
  ]);

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    await handleExport();
  };

  // Restore export flow from URL params (e.g., after auth redirect)
  useEffect(() => {
    const roadmapId = searchParams.get('exportRoadmapId');
    const yearStart = searchParams.get('exportYearStart');
    const quarterName = searchParams.get('exportQuarterName');

    if (roadmapId && yearStart && quarterName && allPlans.length > 0 && isLoggedIn) {
      setSelectedRoadmapId(roadmapId);
      setSelectedYearStart(yearStart);
      setSelectedQuarterName(quarterName);
      setShowModal(true);
      setAutoExport(true);
    }
  }, [searchParams, allPlans.length, isLoggedIn]);

  // Auto-trigger export after restoring from URL params
  useEffect(() => {
    if (autoExport && !saving && selectedRoadmapId && selectedYearStart && selectedQuarterName) {
      setAutoExport(false);
      handleExport();
    }
  }, [autoExport, saving, selectedRoadmapId, selectedYearStart, selectedQuarterName, handleExport]);

  useEffect(() => {
    if (!showModal || allPlans.length === 0) return;

    const alreadySelected = allPlans.find((plan) => String(plan.id) === selectedRoadmapId);
    if (alreadySelected) {
      const selectedYearObj = alreadySelected.content.yearPlans.find(
        (year) => String(year.startYear) === selectedYearStart,
      );

      if (!selectedYearObj) {
        const nextSelection = getDefaultExportSelection(alreadySelected.content.yearPlans);
        setSelectedYearStart(nextSelection.yearStart);
        setSelectedQuarterName(nextSelection.quarterName);
      } else if (!selectedYearObj.quarters.some((q) => q.name === selectedQuarterName)) {
        const nextSelection = getDefaultExportSelection(alreadySelected.content.yearPlans);
        setSelectedQuarterName(nextSelection.quarterName);
      }

      return;
    }

    const roadmap = allPlans.find((plan) => plan.id === currentPlan.id) ?? allPlans[0];
    const nextSelection = getDefaultExportSelection(roadmap.content.yearPlans);
    setSelectedRoadmapId(String(roadmap.id));
    setSelectedYearStart(nextSelection.yearStart);
    setSelectedQuarterName(nextSelection.quarterName);
  }, [showModal, allPlans, currentPlan.id, selectedRoadmapId, selectedYearStart, selectedQuarterName]);

  return (
    <>
      <Button
        className="header-button"
        variant="text"
        size="medium"
        color="inherit"
        startIcon={<IosShare />}
        onClick={() => setShowModal(true)}
      >
        Export
      </Button>
      <Dialog open={showModal} onClose={handleClose} className="changelog-modal" maxWidth="xs" fullWidth>
        <DialogTitle>Export to Scheduler</DialogTitle>
        <DialogContent>
          <Box component="form" noValidate onSubmit={handleSubmit} sx={{ display: 'grid', gap: 2, pt: 1 }}>
            <FormControl fullWidth>
              <InputLabel id="export-roadmap-label">Roadmap</InputLabel>
              <Select
                labelId="export-roadmap-label"
                label="Roadmap"
                value={selectedRoadmapId ?? ''}
                onChange={handleRoadmapChange}
              >
                {allPlans.map((plan) => (
                  <MenuItem key={plan.id} value={String(plan.id)}>
                    {plan.name}
                  </MenuItem>
                ))}
              </Select>
            </FormControl>

            <Box sx={{ display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 2 }}>
              <FormControl fullWidth disabled={roadmapYears.length === 0}>
                <InputLabel id="export-year-label">Year</InputLabel>
                <Select
                  labelId="export-year-label"
                  label="Year"
                  value={selectedYearStart ?? ''}
                  onChange={handleYearChange}
                  renderValue={(val) => {
                    const year = roadmapYears.find((y) => String(y.startYear) === String(val));
                    return year ? <YearDisplayWithRange year={year} /> : '';
                  }}
                >
                  {roadmapYears.map((year) => {
                    const hasCourses = year.quarters.some((q) => (q.courses ?? []).length > 0);
                    return (
                      <MenuItem key={year.startYear} value={String(year.startYear)} disabled={!hasCourses}>
                        <YearDisplayWithRange year={year} />
                      </MenuItem>
                    );
                  })}
                </Select>
              </FormControl>

              <FormControl fullWidth disabled={yearQuarters.length === 0}>
                <InputLabel id="export-quarter-label">Quarter</InputLabel>
                <Select
                  labelId="export-quarter-label"
                  label="Quarter"
                  value={selectedQuarterName ?? ''}
                  onChange={handleQuarterChange}
                  renderValue={(val) => quarterDisplayNames[val as QuarterName] ?? ''}
                >
                  {yearQuarters.map((quarter) => {
                    const isDisabled = (quarter.courses ?? []).length === 0;
                    return (
                      <MenuItem key={quarter.name} value={quarter.name} disabled={isDisabled}>
                        {quarterDisplayNames[quarter.name]}
                      </MenuItem>
                    );
                  })}
                </Select>
              </FormControl>
            </Box>

            {selectedYear && (
              <>
                <Divider></Divider>
                <Box>
                  <strong>Courses to Export: </strong>
                  {selectedYear.quarters
                    .find((q) => q.name === selectedQuarterName)
                    ?.courses?.map((course) => course.id)
                    .join(', ') ?? 'None'}
                </Box>
              </>
            )}
          </Box>
        </DialogContent>
        <DialogActions>
          <Button variant="text" color="inherit" onClick={handleClose}>
            Close
          </Button>
          <Button
            variant="contained"
            color="primary"
            onClick={handleExport}
            disabled={saving || !selectedRoadmap || !selectedYear || !selectedQuarterName}
            startIcon={saving ? <CircularProgress size={18} color="inherit" /> : undefined}
          >
            {saving ? 'Saving…' : 'Export'}
          </Button>
        </DialogActions>
      </Dialog>
    </>
  );
};

export default ExportButton;
