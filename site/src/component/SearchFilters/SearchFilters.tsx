import { FC, KeyboardEvent, useEffect, useState } from 'react';
import './SearchFilters.scss';

import CheckIcon from '@mui/icons-material/Check';
import MenuItem from '@mui/material/MenuItem';
import FormControl from '@mui/material/FormControl';
import ListItemText from '@mui/material/ListItemText';
import Select, { SelectChangeEvent } from '@mui/material/Select';
import { Icon, MenuProps, TextField } from '@mui/material';

import { useAppDispatch, useAppSelector } from '../../store/hooks';

import { FilterOptions, levels } from '../../helpers/searchFilters.ts';
import { selectCourseFilters, setCourseFilters } from '../../store/slices/searchSlice.ts';
import { GE_TITLE_MAP } from '../../helpers/courseRequirements.ts';

const menuProps: Partial<MenuProps> = {
  anchorOrigin: { horizontal: 'left', vertical: 'bottom' },
  transformOrigin: { vertical: 'top', horizontal: 'left' },
  slotProps: {
    paper: { className: 'search-filters-menu' },
  },
};

function DepartmentSelect() {
  const departments = useAppSelector((state) => state.departments.departments);
  const selectedFilters = useAppSelector(selectCourseFilters);
  const dispatch = useAppDispatch();
  const [inputEl, setInputEl] = useState<HTMLInputElement | null>(null);
  const [deptFilterText, setDeptFilterText] = useState('');

  const handleDepartmentSelection = (event: SelectChangeEvent<string[]>) => {
    const {
      target: { value },
    } = event;
    const valueArray = typeof value === 'string' ? value.split(',') : value;

    const validValues = valueArray.filter((val) => val in departments);
    dispatch(setCourseFilters({ ...selectedFilters, departments: validValues }));
  };

  const handleTextKeydown = (event: KeyboardEvent) => {
    const allowedMenuKeys = ['Escape', 'UpArrow', 'DownArrow', 'Enter'];
    if (allowedMenuKeys.includes(event.key)) return;
    event.stopPropagation();
  };

  // Cannot use refs because refs can't be used as hook dependencies
  useEffect(() => {
    inputEl?.focus();
  }, [inputEl]);

  const paperProps = {
    className: 'search-filters-menu departments-menu',
  };

  return (
    <Select
      size="xsmall"
      displayEmpty
      multiple
      value={selectedFilters.departments}
      MenuProps={{
        ...menuProps,
        slotProps: { paper: paperProps },
      }}
      onChange={handleDepartmentSelection}
      renderValue={(selected) => {
        if (selected.length === 0) return 'Departments';
        if (selected.length === 1) return selected[0];

        return `${selected[0]} +${selected.length - 1}`;
      }}
    >
      <TextField
        key="fixed"
        className="dept-input"
        variant="filled"
        placeholder="Enter a department..."
        onKeyDown={handleTextKeydown}
        defaultValue={deptFilterText}
        value={deptFilterText}
        onChange={(event) => setDeptFilterText(event.target.value)}
        inputRef={setInputEl}
      />
      {Object.entries(departments).map(([code, name]) => {
        const labelText = `${code}: ${name}`;
        const hidden = !labelText.toLowerCase().includes(deptFilterText.toLowerCase());

        return (
          <MenuItem key={code} value={code} hidden={hidden} className="search-filter-item">
            {selectedFilters.departments.includes(code) ? <CheckIcon /> : <Icon />}
            <ListItemText className="item-text" primary={labelText} />
          </MenuItem>
        );
      })}
    </Select>
  );
}

const SearchFilters: FC = () => {
  const selectedFilters = useAppSelector(selectCourseFilters);
  const dispatch = useAppDispatch();

  const updateSelectedFilters = (newFilters: FilterOptions) => {
    dispatch(setCourseFilters(newFilters));
  };

  const handleLevelSelection = (event: SelectChangeEvent<string[]>) => {
    const {
      target: { value },
    } = event;

    const valueArray = typeof value === 'string' ? value.split(',') : value;

    updateSelectedFilters({
      ...selectedFilters,
      levels: valueArray,
    });
  };

  const handleGeCategorySelection = (event: SelectChangeEvent<string[]>) => {
    const {
      target: { value },
    } = event;

    const valueArray = typeof value === 'string' ? value.split(',') : value;

    updateSelectedFilters({
      ...selectedFilters,
      geCategories: valueArray,
    });
  };

  // #region MUI Filter Styles
  // Set the height of the tag selector based on the number of tags
  // const ITEM_HEIGHT = 48;
  // const ITEM_PADDING_TOP = 8;

  // Custom styles for the MUI Select and Autocomplete components
  // const selectStyles = {
  //   backgroundColor: 'var(--overlay1)',
  //   borderColor: 'var(--overlay2)',
  //   borderWidth: '1px',
  //   borderRadius: '6px',
  //   '& .MuiOutlinedInput-notchedOutline': {
  //     borderRadius: '6px', // Targets the outer 'fieldset' element
  //   },
  // };

  // const autocompleteStyles = {
  //   // flexGrow: 1,
  //   backgroundColor: 'var(--overlay1)',
  //   borderColor: 'var(--overlay2)',
  //   borderWidth: '1px',
  //   borderRadius: '6px',
  //   '& .MuiOutlinedInput-notchedOutline': {
  //     borderRadius: '6px', // Targets the outer 'fieldset' element
  //   },
  //   width: 'fit-content',
  // };
  // #endregion

  return (
    <div className="filter-group">
      <FormControl className="filter-form-control">
        <Select
          size="xsmall"
          id="level-select"
          multiple
          value={selectedFilters.levels}
          onChange={handleLevelSelection}
          displayEmpty
          variant="outlined"
          renderValue={(selected) => {
            return selected.length === 0 ? 'Level' : selected.join(', ');
            // return <p className="filter-value">{selected.length === 0 ? 'Level' : selected.join(', ')}</p>;
          }}
          MenuProps={menuProps}
        >
          {Object.keys(levels).map((key) => (
            <MenuItem key={key} value={levels[key]} className="search-filter-item">
              {selectedFilters.levels.includes(levels[key]) ? <CheckIcon /> : <span style={{ width: 24 }}></span>}
              <ListItemText primary={levels[key]} />
            </MenuItem>
          ))}
        </Select>
      </FormControl>
      <FormControl className="filter-form-control">
        <Select
          size="xsmall"
          id="ge-category-select"
          multiple
          value={selectedFilters.geCategories}
          onChange={handleGeCategorySelection}
          displayEmpty
          renderValue={(selected) => {
            return selected.length === 0 ? 'GE' : selected.join(', ');
            // return <p className="filter-value">{selected.length === 0 ? 'GE' : selected.join(', ')}</p>;
          }}
          MenuProps={menuProps}
        >
          {Object.entries(GE_TITLE_MAP).map(([key, value]) => (
            <MenuItem key={key} value={key} className="search-filter-item">
              {selectedFilters.geCategories.includes(key) ? <CheckIcon /> : <Icon />}
              <ListItemText primary={value} />
            </MenuItem>
          ))}
        </Select>
      </FormControl>
      <DepartmentSelect />
      {/* <Autocomplete
        multiple
        size="xsmall"
        limitTags={1}
        id="multiple-limit-tags"
        options={Object.keys(departments)}
        value={selectedFilters.departments}
        onChange={(_event, newValue) => {
          updateSelectedFilters({
            ...selectedFilters,
            departments: newValue,
          });
        }}
        getOptionLabel={(option) => option}
        renderOption={(props, option) => <li {...props}>{departments[option]}</li>}
        renderInput={(params) => (
          <TextField {...params} placeholder={selectedFilters.departments.length === 0 ? 'Department' : ''} />
        )}
        clearIcon={null}
        sx={autocompleteStyles}
      /> */}
    </div>
  );
};

export default SearchFilters;
