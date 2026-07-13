import './CatalogYears.scss';

import { FC } from 'react';
import { CATALOG_YEAR_OPTIONS, DEFAULT_CATALOG_YEAR } from '../../../helpers/courseRequirements';

import { FormControl, MenuItem, Select, SelectChangeEvent, Tooltip } from '@mui/material';
import KeyboardArrowDownIcon from '@mui/icons-material/KeyboardArrowDown';

interface CatalogYearsProps {
  catalogYear: string | null;
  tab: 'Major' | 'Minor';
  onChange: (event: SelectChangeEvent) => Promise<void>;
}

const CatalogYears: FC<CatalogYearsProps> = ({ catalogYear, tab, onChange }) => {
  return (
    <>
      <Tooltip
        title={`${tab} requirements from a specific catalog year`}
        placement="bottom-start"
        slotProps={{
          tooltip: { className: 'catalog-year-tooltip' },
          popper: {
            modifiers: [{ name: 'offset', options: { offset: [0, -8] } }],
          },
        }}
        disableInteractive
      >
        <h5 className="catalog-year-title">Catalog Year</h5>
      </Tooltip>
      <FormControl className="catalog-year-dropdown" fullWidth>
        <Select
          IconComponent={KeyboardArrowDownIcon}
          labelId="catalog-year-select-label"
          id="catalog-year-select"
          value={catalogYear ?? DEFAULT_CATALOG_YEAR}
          onChange={onChange}
        >
          {CATALOG_YEAR_OPTIONS.map((option) => (
            <MenuItem key={option.value} value={option.value}>
              {option.label}
            </MenuItem>
          ))}
        </Select>
      </FormControl>
    </>
  );
};

export default CatalogYears;
