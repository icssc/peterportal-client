import { geCategories, geCodes } from 'peterportal-api-next-types';
import { useState, useEffect, FC } from 'react';
import Dropdown from 'react-bootstrap/Dropdown';

interface SearchFilterProps {
  onGEChange: (selectedGE: string) => void;
}

const SearchFilter: FC<SearchFilterProps> = ({ onGEChange }) => {
  const [geDisplay, setGEDisplay] = useState<string>("All: Don't Filter For GE");

  const onClick = (geCategory: string, geCode: string) => {
    onGEChange(geCode);
    setGEDisplay(geCategory === '' ? "All: Don't Filter For GE" : geCategory);
  };

  return (
    <div className="search-filter">
      <Dropdown className="search-filter-ge">
        <Dropdown.Toggle>{geDisplay}</Dropdown.Toggle>

        <Dropdown.Menu>
          <Dropdown.Item onClick={() => onClick('')}>All: Don't Filter for GE</Dropdown.Item>
          {geCategories.map((geCategory, i) => {
            return (
              <Dropdown.Item onClick={() => onClick(geCategory, geCodes[i])} key={geCodes[i]}>
                {geCategory}
              </Dropdown.Item>
            );
          })}
        </Dropdown.Menu>
      </Dropdown>
    </div>
  );
};

export default SearchFilter;
