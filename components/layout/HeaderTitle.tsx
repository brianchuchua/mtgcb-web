import styled from 'styled-components';
import Typography from '@material-ui/core/Typography';

// Reason: https://github.com/mui-org/material-ui/issues/20373
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const HeaderTitle = styled(Typography as any)<HeaderTitleProps>(() => ({ flexGrow: 1 }));

interface HeaderTitleProps {
  flexGrow: number;
}

export default HeaderTitle;
