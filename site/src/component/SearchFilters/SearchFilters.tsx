import { FC } from 'react';
import './SearchFilters.scss';

import CheckIcon from '@mui/icons-material/Check';
import MenuItem from '@mui/material/MenuItem';
import FormControl from '@mui/material/FormControl';
import ListItemText from '@mui/material/ListItemText';
import Select, { SelectChangeEvent } from '@mui/material/Select';
import Autocomplete from '@mui/material/Autocomplete';
import TextField from '@mui/material/TextField';
import { Icon } from '@mui/material';

import { useAppSelector } from '../../store/hooks';

import { FilterOptions, levels, geCategories } from '../../helpers/searchFilters.ts';

interface SearchFiltersProps {
  selectedFilters: FilterOptions;
  updateSelectedFilters: (value: FilterOptions) => void;
}

const SearchFilters: FC<SearchFiltersProps> = ({ selectedFilters, updateSelectedFilters }) => {
  const departments = useAppSelector((state) => state.departments.departments);

  const handleLevelSelection = (event: SelectChangeEvent<typeof selectedFilters.levels>) => {
    const {
      target: { value },
    } = event;

    const valueArray = typeof value === 'string' ? value.split(',') : value;

    updateSelectedFilters({
      ...selectedFilters,
      levels: valueArray,
    });
  };

  const handleGeCategorySelection = (event: SelectChangeEvent<typeof selectedFilters.geCategories>) => {
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
  const ITEM_HEIGHT = 48;
  const ITEM_PADDING_TOP = 8;
  const MenuProps = {
    PaperProps: {
      style: {
        maxHeight: ITEM_HEIGHT * 4.5 + ITEM_PADDING_TOP,
        width: 250,
      },
    },
  };

  // Custom styles for the MUI Select and Autocomplete components
  const selectStyles = {
    backgroundColor: 'var(--overlay1)',
    borderColor: 'var(--overlay2)',
    borderWidth: '1px',
    borderRadius: '6px',
    '& .MuiOutlinedInput-notchedOutline': {
      borderRadius: '6px', // Targets the outer 'fieldset' element
    },
  };

  const autocompleteStyles = {
    // flexGrow: 1,
    backgroundColor: 'var(--overlay1)',
    borderColor: 'var(--overlay2)',
    borderWidth: '1px',
    borderRadius: '6px',
    '& .MuiOutlinedInput-notchedOutline': {
      borderRadius: '6px', // Targets the outer 'fieldset' element
    },
    width: 'fit-content',
  };
  // #endregion

  return (
    <div className="filter-group">
      <FormControl sx={selectStyles}>
        <Select
          size="xsmall"
          id="level-select"
          multiple
          value={selectedFilters.levels}
          onChange={handleLevelSelection}
          displayEmpty
          renderValue={(selected) => {
            return <p className="filter-value">{selected.length === 0 ? 'Level' : selected.join(', ')}</p>;
          }}
          MenuProps={MenuProps}
        >
          {Object.keys(levels).map((key) => (
            <MenuItem key={key} value={levels[key]}>
              {selectedFilters.levels.includes(levels[key]) ? <CheckIcon /> : <span style={{ width: 24 }}></span>}
              <ListItemText primary={levels[key]} />
            </MenuItem>
          ))}
        </Select>
      </FormControl>
      <FormControl sx={selectStyles}>
        <Select
          size="xsmall"
          id="ge-category-select"
          multiple
          value={selectedFilters.geCategories}
          onChange={handleGeCategorySelection}
          displayEmpty
          renderValue={(selected) => {
            return <p className="filter-value">{selected.length === 0 ? 'GE' : selected.join(', ')}</p>;
          }}
          MenuProps={MenuProps}
        >
          {Object.keys(geCategories).map((key) => (
            <MenuItem key={key} value={key}>
              {selectedFilters.geCategories.includes(key) ? <CheckIcon /> : <Icon />}
              <ListItemText primary={`${key}: ${geCategories[key]}`} />
            </MenuItem>
          ))}
        </Select>
      </FormControl>
      <Autocomplete
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
      />
    </div>
  );
};

export default SearchFilters;
