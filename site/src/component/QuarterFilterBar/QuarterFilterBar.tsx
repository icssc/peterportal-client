'use client';
import './QuarterFilterBar.scss';
import { FC } from 'react';
import { useAppDispatch, useAppSelector } from '../../store/hooks';
import { QuarterFilterName } from '../../helpers/quarterFilter';
import { toggleQuarterFilter } from '../../store/slices/searchSlice';

const QUARTERS: { name: QuarterFilterName; emoji: string }[] = [
  { name: 'Fall', emoji: '🍂' },
  { name: 'Winter', emoji: '❄️' },
  { name: 'Spring', emoji: '🌸' },
  { name: 'Summer', emoji: '☀️' },
];

const QuarterFilterBar: FC = () => {
  const dispatch = useAppDispatch();
  const quarterFilters = useAppSelector((state) => state.search.quarterFilters);

  return (
    <div className="quarter-filter-bar">
      {QUARTERS.map(({ name, emoji }) => (
        <button
          key={name}
          className={`quarter-filter-btn${quarterFilters.includes(name) ? ' active' : ''}`}
          onClick={() => dispatch(toggleQuarterFilter(name))}
          title={`Filter by ${name}`}
        >
          <span className="quarter-emoji">{emoji}</span>
          <span className="quarter-name">{name}</span>
        </button>
      ))}
    </div>
  );
};

export default QuarterFilterBar;
