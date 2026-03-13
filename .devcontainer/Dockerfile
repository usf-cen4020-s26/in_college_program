FROM ubuntu:22.04

# Avoid interactive tzdata, etc.
ENV DEBIAN_FRONTEND=noninteractive

# Install GNU COBOL, Python, and useful tools
RUN apt-get update && \
	apt-get install -y --no-install-recommends \
		gnucobol \
		build-essential \
		gdb \
		make \
		git \
		locales \
		ca-certificates \
		curl \
		gnupg \
		python3 \
		python3-pip && \
	rm -rf /var/lib/apt/lists/*

# Install Node.js 20 from NodeSource
RUN install -d -m 0755 /etc/apt/keyrings && \
	curl -fsSL https://deb.nodesource.com/gpgkey/nodesource-repo.gpg.key | gpg --dearmor -o /etc/apt/keyrings/nodesource.gpg && \
	echo "deb [signed-by=/etc/apt/keyrings/nodesource.gpg] https://deb.nodesource.com/node_20.x nodistro main" > /etc/apt/sources.list.d/nodesource.list && \
	apt-get update && \
	apt-get install -y --no-install-recommends nodejs && \
	rm -rf /var/lib/apt/lists/*

# Configure UTF-8 locale
RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Create workspace
WORKDIR /workspace

# Default command
CMD ["/bin/bash"]
