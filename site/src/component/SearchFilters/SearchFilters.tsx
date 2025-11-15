import { FC } from 'react';
import './SearchFilters.scss';

import CheckIcon from '@mui/icons-material/Check';
import MenuItem from '@mui/material/MenuItem';
import FormControl from '@mui/material/FormControl';
import ListItemText from '@mui/material/ListItemText';
import Select, { SelectChangeEvent } from '@mui/material/Select';
import { Icon, MenuProps } from '@mui/material';

import { useAppDispatch, useAppSelector } from '../../store/hooks';

import { FilterOptions, levels } from '../../helpers/searchFilters.ts';
import { selectCourseFilters, setCourseFilters } from '../../store/slices/searchSlice.ts';
import { GE_TITLE_MAP } from '../../helpers/courseRequirements.ts';

const SearchFilters: FC = () => {
  const departments = useAppSelector((state) => state.departments.departments);
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

  const handleDepartmentSelection = (event: SelectChangeEvent<string[]>) => {
    const {
      target: { value },
    } = event;
    const valueArray = typeof value === 'string' ? value.split(',') : value;

    updateSelectedFilters({ ...selectedFilters, departments: valueArray });
  };

  // #region MUI Filter Styles
  // Set the height of the tag selector based on the number of tags
  // const ITEM_HEIGHT = 48;
  // const ITEM_PADDING_TOP = 8;
  const MenuProps: Partial<MenuProps> = {
    anchorOrigin: { horizontal: 'left', vertical: 'bottom' },
    transformOrigin: { vertical: 'top', horizontal: 'left' },
    PaperProps: {
      style: {
        // maxHeight: ITEM_HEIGHT * 4.5 + ITEM_PADDING_TOP,
        // width: 250,
      },
    },
  };

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

  function DepartmentSelect() {
    return (
      <Select
        size="xsmall"
        displayEmpty
        multiple
        value={selectedFilters.departments}
        MenuProps={MenuProps}
        onChange={handleDepartmentSelection}
        renderValue={(selected) => {
          return selected.length === 0 ? 'Departments' : selected.join(', ');
        }}
      >
        {/* <TextField variant="filled" placeholder="Enter a department..." /> */}
        {Object.entries(departments).map(([code, name]) => (
          <MenuItem key={code} value={name}>
            {selectedFilters.departments.includes(code) ? <CheckIcon /> : <Icon />}
            <ListItemText primary={name} />
          </MenuItem>
        ))}
      </Select>
    );
  }

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
          MenuProps={{
            anchorOrigin: {
              horizontal: 'left',
              vertical: 'bottom',
            },
            transformOrigin: {
              vertical: 'top',
              horizontal: 'left',
            },
          }}
        >
          {Object.keys(levels).map((key) => (
            <MenuItem key={key} value={levels[key]}>
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
          MenuProps={MenuProps}
        >
          {Object.entries(GE_TITLE_MAP).map(([key, value]) => (
            <MenuItem key={key} value={key}>
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
