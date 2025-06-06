import { IconButton, IconButtonProps, styled } from '@mui/material';

interface ExpandMoreProps extends IconButtonProps {
  expand: boolean;
}

export const ExpandMore = styled((props: ExpandMoreProps) => <IconButton {...props} />)(({ theme }) => ({
  marginLeft: 'auto',
  transition: theme.transitions.create('transform', {
    duration: theme.transitions.duration.shortest,
  }),
  variants: [
    {
      props: ({ expand }) => !expand,
      style: { transform: 'rotate(0deg)' },
    },
    {
      props: ({ expand }) => !!expand,
      style: { transform: 'rotate(180deg)' },
    },
  ],
}));
