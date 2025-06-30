import { IconButton, IconButtonProps, styled } from '@mui/material';
import ExpandMoreIcon from '@mui/icons-material/ExpandMore';

interface ExpandMoreProps extends IconButtonProps {
  expanded: boolean;
}

export const ExpandMore = styled((props: ExpandMoreProps) => (
  <IconButton {...props}>
    <ExpandMoreIcon />
  </IconButton>
))(({ theme }) => ({
  marginLeft: 'auto',
  transition: theme.transitions.create('transform', {
    duration: theme.transitions.duration.shortest,
  }),
  variants: [
    {
      props: ({ expanded }) => !expanded,
      style: { transform: 'rotate(0deg)' },
    },
    {
      props: ({ expanded }) => !!expanded,
      style: { transform: 'rotate(180deg)' },
    },
  ],
}));
