import Container from '@material-ui/core/Container';
import styled from 'styled-components';

export const ResponsiveContainer = styled(Container)(({ theme }) => ({
  [theme.breakpoints.down('md')]: {
    padding: 0,
  },
}));
