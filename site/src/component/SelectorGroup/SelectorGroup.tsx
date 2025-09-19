import { FC, ReactNode } from 'react';
import './SelectorGroup.scss';
import ToggleButton from '@mui/material/ToggleButton';
import ToggleButtonGroup from '@mui/material/ToggleButtonGroup';

interface SelectorGroupProps {
  tabList: { name: string; icon?: ReactNode }[];
  selectedTab: string;
  className?: string;
  selectTab: (clickedTab: string) => void;
}

const SelectorButton: FC<{ name: string; icon?: React.ReactNode; selected: boolean; onClick: () => void }> = ({
  name,
  icon,
  selected,
  onClick,
}) => (
  <ToggleButton value={name} className="ppc-btn" selected={selected} onClick={onClick}>
    <div className="button-content">
      {icon}
      <span>{name}</span>
    </div>
  </ToggleButton>
);

/**
 * A reusable toggle button group that displays a list of selectable tabs,
 * optionally with icons.
 *
 * Props:
 * @param tabList - An array of objects representing the tabs. Each object has:
 *    - name: string → The label of the tab.
 *    - icon?: React.ReactNode → Optional JSX element to display next to the tab label.
 * @param selectedTab - string → The currently selected tab's name.
 * @param selectTab - function → Callback invoked when a tab is clicked; receives the clicked tab's name as an argument.
 * @param className - string? → Additional CSS class(es) to apply to the ToggleButtonGroup for custom styling.
 */
const SelectorGroup: FC<SelectorGroupProps> = ({
  tabList,
  selectedTab,
  selectTab,
  className = '',
}: SelectorGroupProps) => {
  return (
    <ToggleButtonGroup className={`selector-group ${className}`}>
      {tabList.map(({ name, icon }) => (
        <SelectorButton
          key={name}
          name={name}
          icon={icon}
          selected={selectedTab === name}
          onClick={() => selectTab(name)}
        />
      ))}
    </ToggleButtonGroup>
  );
};

export default SelectorGroup;
