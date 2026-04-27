import { FC, useCallback, useEffect, useMemo, useState } from 'react';
import { Autocomplete, FilterOptionsState, TextField } from '@mui/material';
import trpc from '../../../trpc';
import { normalizeMajorName } from '../../../helpers/courseRequirements';
import { addMinor, removeMinor, setMinorList, MinorRequirements } from '../../../store/slices/courseRequirementsSlice';
import { useAppDispatch, useAppSelector } from '../../../store/hooks';
import { MinorProgram } from '@peterportal/types';
import { useIsLoggedIn } from '../../../hooks/isLoggedIn';
import MinorCourseList from './MinorCourseList';

function updateSelectedMinors(minorIds: string[]) {
  trpc.programs.saveSelectedMinor.mutate({ minorIds });
}

interface MinorOption {
  value: MinorProgram;
  label: string;
}

const MinorSelector: FC = () => {
  const isLoggedIn = useIsLoggedIn();
  const minors = useAppSelector((state) => state.courseRequirements.minorList);
  const selectedMinors = useAppSelector((state) => state.courseRequirements.selectedMinors);

  const [minorsLoading, setMinorsLoading] = useState(false);

  const dispatch = useAppDispatch();

  // Initial Load Helpers
  const saveInitialMinorList = useCallback(
    (minors: MinorProgram[]) => {
      for (const m of minors) m.name = normalizeMajorName(m);
      minors.sort((a, b) => a.name.localeCompare(b.name));
      setMinorsLoading(false);
      dispatch(setMinorList(minors));
    },
    [dispatch],
  );

  // Initial Load, fetch minors
  useEffect(() => {
    if (minors.length) return;
    trpc.programs.getMinors.query().then(saveInitialMinorList);
  }, [dispatch, minors.length, saveInitialMinorList]);

  const saveMinors = useCallback(
    (minorsToSave: MinorRequirements[]) => {
      if (!isLoggedIn) return;
      const minorIds: string[] = minorsToSave.map((m) => m.minor.id);
      updateSelectedMinors(minorIds);
    },
    [isLoggedIn],
  );

  const handleMinorChange = useCallback(
    (_event: unknown, selections: MinorOption[] | null) => {
      const newMinors = selections?.map((s) => s.value) || [];
      const currentMinorIds = selectedMinors.map((m) => m.minor.id);

      currentMinorIds.forEach((id) => {
        if (!newMinors.find((m) => m.id === id)) dispatch(removeMinor(id));
      });
      newMinors.forEach((minor) => {
        if (!currentMinorIds.includes(minor.id)) dispatch(addMinor(minor));
      });

      const updatedMinors = newMinors.map((minor) => ({
        minor,
        requirements: selectedMinors.find((m) => m.minor.id === minor.id)?.requirements || [],
      }));
      saveMinors(updatedMinors);
    },
    [dispatch, saveMinors, selectedMinors],
  );

  useEffect(() => {
    if (!minors.length || !isLoggedIn) return;
    if (selectedMinors.length) return;

    setMinorsLoading(true);
    trpc.programs.getSavedMinors
      .query()
      .then((minorIds) => {
        for (const minor of minorIds) {
          const foundMinor = minors.find((m) => m.id === minor.id);
          if (!foundMinor) continue;
          dispatch(addMinor(foundMinor));
        }
      })
      .finally(() => {
        setMinorsLoading(false);
      });
  }, [dispatch, minors, isLoggedIn, selectedMinors.length]);

  const minorSelectOptions: MinorOption[] = minors.map((m) => ({
    value: m,
    label: `${m.name}`,
  }));

  const getAbbreviation = (name: string): string => {
    return name
      .slice(9)
      .split(' ')
      .filter((word) => word[0] !== word[0].toLowerCase() && word[0] === word[0].toUpperCase())
      .map((word) => word[0])
      .join('');
  };

  const minorAbbreviations = useMemo(() => {
    const map: Record<string, string[]> = {};
    for (const minor of minors) {
      const abbr = getAbbreviation(minor.name);
      if (!map[abbr]) map[abbr] = [];
      map[abbr].push(minor.name);
    }
    return map;
  }, [minors]);

  const filterMinorOptions = (options: MinorOption[], state: FilterOptionsState<MinorOption>) => {
    const input = state.inputValue.trim().toUpperCase();

    // list of minors that match abbreviation
    const minorAbbrMatches: string[] = [];

    if (input) {
      for (const [abbr, fullName] of Object.entries(minorAbbreviations)) {
        if (abbr.startsWith(input)) {
          minorAbbrMatches.push(...fullName);
        }
      }
    }
    const abbrFiltered = options.filter((option) =>
      minorAbbrMatches.some((term) => option.label.toLowerCase().includes(term.toLowerCase())),
    );

    // list of filtered minors
    const filtered = options.filter(
      (option) =>
        option.label.toLowerCase().includes(state.inputValue.toLowerCase()) &&
        !abbrFiltered.some((a) => a.value.id === option.value.id),
    );

    // abbreviated matches first
    return [...abbrFiltered, ...filtered];
  };

  return (
    <>
      <Autocomplete
        multiple
        options={minorSelectOptions}
        value={selectedMinors.map((m) => minorSelectOptions.find((o) => o.value.id === m.minor.id)!).filter(Boolean)}
        onChange={handleMinorChange}
        getOptionLabel={(option) => option.label}
        getOptionKey={(option) => option.value.id}
        isOptionEqualToValue={(option, value) => option.value.id === value.value.id}
        filterOptions={filterMinorOptions}
        loading={minorsLoading}
        disabled={minorsLoading}
        disableClearable
        className="minor-select"
        renderInput={(params) => (
          <TextField
            {...params}
            variant="outlined"
            size="small"
            placeholder={selectedMinors.length === 0 ? 'Select minors...' : undefined}
          />
        )}
      />
      {selectedMinors.map((data) => (
        <MinorCourseList key={data.minor.id} minorReqs={data} />
      ))}
    </>
  );
};

export default MinorSelector;
