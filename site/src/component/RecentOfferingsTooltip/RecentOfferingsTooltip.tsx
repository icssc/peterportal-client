import { FC } from 'react';
import './RecentOfferingsTooltip.scss';
import { OverlayTrigger, Popover } from 'react-bootstrap';
import RecentOfferingsTable from '../RecentOfferingsTable/RecentOfferingsTable';

interface RecentOfferingsTooltipProps {
  terms: string[];
}

/*

- improve styling
  - center the "Recent Offerings" title
  - fix the tooltip's positioning
  - fix the bug w/ the tooltip arrow showing up in the wrong place
  - make sure it looks good on mobile
  - make sure the emojis in this PR are the same size as on prod

- for courses offered more than 4 years ago, show, desending vertically:
  1. the current year
  2. a vertical ellipsis
  3. the 2 most recent years it was offered

- if terms.length is less than 4, add empty rows to the table

*/

const RecentOfferingsTooltip: FC<RecentOfferingsTooltipProps> = ({ terms }) => {
  const prevYear = new Date().getMonth() > 9 ? new Date().getFullYear() : new Date().getFullYear() - 1;
  const prevOfferings = [];

  // if the course was offered in the previous academic year, add the corresponding emoji to prevOfferings
  if (terms.includes(`${prevYear - 1} Fall`)) prevOfferings.push('🍂');
  if (terms.includes(`${prevYear} Winter`)) prevOfferings.push('❄️');
  if (terms.includes(`${prevYear} Spring`)) prevOfferings.push('🌸');
  if (terms.some((t) => t.startsWith(`${prevYear} Summer`))) prevOfferings.push('☀️');

  // if the course was not offered in the previous academic year, show a ❌
  if (prevOfferings.length === 0) prevOfferings.push('❌');

  const popover = (
    <Popover id="recent-offerings-popover">
      <Popover.Content className="recent-offerings-tooltip">
        <RecentOfferingsTable terms={terms} size="thin" />
      </Popover.Content>
    </Popover>
  );

  return (
    <div className="tooltip-container">
      <OverlayTrigger overlay={popover} placement="auto">
        <div className="tooltip-trigger">
          {prevOfferings.map((emoji) => (
            <span key={emoji}>{emoji}</span>
          ))}
        </div>
      </OverlayTrigger>
    </div>
  );
};

export default RecentOfferingsTooltip;
