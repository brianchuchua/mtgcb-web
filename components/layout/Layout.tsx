import React from 'react';
import styled from 'styled-components';
import Header from './Header';

const OuterWrapper = styled.div({
  display: 'flex',
});

const ContentWrapper = styled.main(({ theme }) => ({
  flexGrow: 1,
  height: 'calc(100vh-64px)',
  overflow: 'auto',
  backgroundColor: theme.palette.background.default,
  padding: theme.spacing(3),
}));

const ToolbarSpacer = styled.div(({ theme }) => ({
  ...theme.mixins.toolbar,
}));

const Layout: React.FC = ({ children }) => (
  <OuterWrapper>
    <Header />
    <ContentWrapper>
      <ToolbarSpacer />
      {children}
    </ContentWrapper>
  </OuterWrapper>
);

export default Layout;
